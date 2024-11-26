module Control.Monad.Yield.Class
  ( MonadYield (..)
  ) where

import Control.Monad (Monad)
import Data.Kind (Type)

class Monad m => MonadYield (a :: Type) (m :: Type -> Type) | m -> a where
  yield :: a -> m ()
