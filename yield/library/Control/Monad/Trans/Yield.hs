{-# LANGUAGE UndecidableInstances #-}

module Control.Monad.Trans.Yield
  ( YieldT (..)
  , runYieldT
  , hoistYieldT
  ) where

import Prelude

import Control.Monad (ap, liftM)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Reader (MonadReader (..))
import Control.Monad.State (MonadState (..))
import Control.Monad.Trans.Class (MonadTrans (..))
import Control.Monad.Trans.Codensity (CodensityT (..))
import Control.Monad.Yield.Aggregation (Aggregation (..))
import Control.Monad.Yield.Class (MonadYield (..))

-- | Monad transformer that adds dynamically interpretable 'MonadYield' support
newtype YieldT a m r = YieldT (CodensityT (Yielder a m) r)
  deriving newtype (Functor, Applicative, Monad)

instance MonadTrans (YieldT a) where
  lift = YieldT . lift . lift

deriving newtype instance MonadIO m => MonadIO (YieldT a m)

deriving newtype instance Functor m => MonadYield a (YieldT a m)

deriving newtype instance MonadReader r m => MonadReader r (YieldT a m)

deriving newtype instance MonadState s m => MonadState s (YieldT a m)

-- | Transforms a 'YieldT''s base monad actions
hoistYieldT
  :: forall m n a r
   . Monad m
  => (forall x. m x -> n x)
  -> YieldT a m r
  -> YieldT a n r
hoistYieldT f (YieldT (CodensityT y0)) = YieldT $ CodensityT \continue ->
  let go = \case
        Pure r -> continue r
        Yield a y -> Yield a (go y)
        Act m -> Act (f $ liftM go $ collapse m)
         where
          collapse mpipe = do
            pipe' <- mpipe
            case pipe' of
              Act mpipe' -> collapse mpipe'
              _ -> pure pipe'
  in  go $ y0 Pure

-- | Runs a 'YieldT' computation in the base monad
runYieldT
  :: forall a b m r
   . Monad m
  => Aggregation m a b
  -- ^ What to do with the values emitted from the stream
  -> YieldT a m r
  -> m (b, r)
  -- ^ The result of aggregating of stream values, and the result of the 'YieldT'
runYieldT agg (YieldT (CodensityT y)) = runYielder agg (y Pure)

data Yielder a m r
  = Yield a (Yielder a m r)
  | Act (m (Yielder a m r))
  | Pure r
  deriving stock (Functor)

instance Functor m => Applicative (Yielder a m) where
  pure = Pure
  (<*>) = ap

instance Functor m => Monad (Yielder a m) where
  p0 >>= f = go p0
   where
    go p = case p of
      Yield a continue -> Yield a $ go continue
      Act m -> Act (go <$> m)
      Pure r -> f r

instance Functor m => MonadYield a (Yielder a m) where
  yield x = Yield x (Pure ())

instance MonadTrans (Yielder a) where
  lift = Act . fmap Pure

instance MonadIO m => MonadIO (Yielder a m) where
  liftIO = lift . liftIO

instance MonadReader r m => MonadReader r (Yielder a m) where
  ask = lift ask
  local f = hoistYielder $ local f

instance MonadState s m => MonadState s (Yielder a m) where
  state = lift . state

hoistYielder
  :: Functor n => (forall x. m x -> n x) -> Yielder a m r -> Yielder a n r
hoistYielder f p0 = go p0
 where
  go = \case
    Yield a p -> Yield a $ go p
    Act m -> Act $ go <$> f m
    Pure r -> Pure r

-- | Runs a 'Yielder' computation in the base monad
runYielder
  :: Monad m
  => Aggregation m a b
  -- ^ What to do with the values emitted from the stream
  -> Yielder a m r
  -> m (b, r)
  -- ^ The result of aggregating of stream values, and the result of the 'Yielder'
runYielder Aggregation {begin, step, done} p0 = begin >>= go p0
 where
  go p x = case p of
    Yield a continue -> (go continue $!) =<< step x a
    Act m -> m >>= \p' -> go p' x
    Pure r -> (,r) <$> done x
