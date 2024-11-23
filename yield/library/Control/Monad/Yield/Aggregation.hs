module Control.Monad.Yield.Aggregation
  ( Aggregation (..)
  , noAggregation
  , seqAggregation
  , listAggregation
  ) where

import Data.Foldable (toList)
import Data.Sequence (Seq (..))
import Prelude (Applicative (..), Functor (..), (.), (<$>))

-- | An effectful stream consumer
--
-- The type parameter @x@ represents the consumer's internal state.
data Aggregation m a b = forall x.
  Aggregation
  { begin :: m x
  -- ^ Produce the initial state
  , step :: x -> a -> m x
  -- ^ Consume one item from the stream
  , done :: x -> m b
  -- ^ Produce the final result
  }

instance Functor m => Functor (Aggregation m a) where
  fmap f Aggregation {begin, step, done} =
    Aggregation {begin, step, done = fmap f . done}

noAggregation :: Applicative m => Aggregation m a ()
noAggregation = Aggregation {begin = pure (), step = \() _ -> pure (), done = pure}

seqAggregation :: Applicative m => Aggregation m a (Seq a)
seqAggregation = Aggregation {begin = pure Empty, step = \xs -> pure . (xs :|>), done = pure}

listAggregation :: Applicative m => Aggregation m a [a]
listAggregation = toList <$> seqAggregation
