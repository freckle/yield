# Yield

<!--
```haskell
module Main (main) where

import Prelude
import Text.Markdown.Unlit ()
import Test.Hspec
```
-->

```haskell
import Control.Monad.Yield
```

```haskell
action1 :: MonadYield Char m => m Char
action1 = do
  yield 'a'
  yield 'b'
  pure 'c'
```

```haskell
action2 :: forall m. Monad m => m (String, Char)
action2 = runYieldT listAggregation action1
-- returns ("ab", 'c')
```

<!--
```haskell
main :: IO ()
main = hspec do
  it "" do
    action2 `shouldReturn` ("ab", 'c')
```
-->

---

[LICENSE](./LICENSE) | [CHANGELOG](./CHANGELOG.md)
