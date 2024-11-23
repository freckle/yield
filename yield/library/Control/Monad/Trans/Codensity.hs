{-# LANGUAGE UndecidableInstances #-}

module Control.Monad.Trans.Codensity
  ( CodensityT (..)
  ) where

import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Reader (MonadReader (ask, local))
import Control.Monad.State (MonadState (state))
import Control.Monad.Trans.Class (MonadTrans (..))
import Control.Monad.Yield.Class (MonadYield (..))
import Data.Kind (Type)
import Prelude (Applicative (..), Functor (..), Monad (..), const, ($), (.))

-- | The "codensity transform", taken as an excerpt from the @kan-extensions@
--   package, following the model of @conduit@ to permit efficient monadic bind
newtype CodensityT (m :: Type -> Type) (a :: Type) = CodensityT {runCodensity :: forall b. (a -> m b) -> m b}

instance Functor (CodensityT m) where
  fmap f (CodensityT m) = CodensityT (\k -> m (k . f))

instance Applicative (CodensityT m) where
  pure x = CodensityT (\k -> k x)
  CodensityT f <*> CodensityT g = CodensityT (\bfr -> f (\ab -> g (bfr . ab)))

instance Monad (CodensityT m) where
  m >>= k = CodensityT (\c -> runCodensity m (\a -> runCodensity (k a) c))

instance MonadTrans CodensityT where
  lift m = CodensityT (m >>=)

instance MonadIO m => MonadIO (CodensityT m) where
  liftIO = lift . liftIO

instance MonadYield a m => MonadYield a (CodensityT m) where
  yield = lift . yield

instance MonadReader r m => MonadReader r (CodensityT m) where
  ask = CodensityT (ask >>=)
  local f m = CodensityT $ \c -> ask >>= \r -> local f . runCodensity m $ local (const r) . c

instance MonadState s m => MonadState s (CodensityT m) where
  state = lift . state
