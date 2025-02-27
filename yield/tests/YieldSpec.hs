module YieldSpec (spec) where

import Prelude

import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Yield
import Data.Foldable
import Test.Hspec

spec :: Spec
spec = describe "YieldT" do
  describe "Trivial examples" do
    it "" $ runYieldT noAggregation (pure 'x') `shouldReturn` ((), 'x')
    it "" $ runYieldT listAggregation (pure 'x') `shouldReturn` ("", 'x')
    it ""
      $ runYieldT listAggregation (yield 'a' >> pure 'x') `shouldReturn` ("a", 'x')
    it ""
      $ runYieldT noAggregation (yield 'a' >> pure 'x') `shouldReturn` ((), 'x')
    it ""
      $ runYieldT listAggregation (yield 'a' >> yield 'b' >> pure 'c')
        `shouldReturn` ("ab", 'c')

  describe "Monad instance" do
    it ""
      $ runYieldT
        listAggregation
        (traverse_ @[] yield "ab" >> traverse @[] yield "cd" >> pure 'x')
        `shouldReturn` ("abcd", 'x')

  describe "MonadReader instance" do
    it "" $ runReaderT (runYieldT noAggregation ask) 'a' `shouldReturn` ((), 'a')
    it ""
      $ runReaderT (runYieldT noAggregation (local succ ask)) 'a'
        `shouldReturn` ((), 'b')
    it ""
      $ runReaderT
        ( runYieldT listAggregation do
            ask >>= yield
            local succ do
              ask >>= yield
              local succ do
                ask >>= yield
            ask >>= yield
        )
        'g'
        `shouldReturn` ("ghig", ())

  describe "MonadState instance" do
    it "" $ evalStateT (runYieldT noAggregation get) 'a' `shouldReturn` ((), 'a')
    it ""
      $ evalStateT (runYieldT noAggregation (modify' succ >> get)) 'a'
        `shouldReturn` ((), 'b')
    it ""
      $ evalStateT
        ( do
            (a, _) <- runYieldT listAggregation do
              get >>= yield
              modify' succ
              get >>= yield
              put 'x'
              get >>= yield
            gets (a,)
        )
        'g'
        `shouldReturn` ("ghx", 'x')
