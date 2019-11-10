module Main
  ( main
  ) where

import Data.Proxy (Proxy(Proxy))
import qualified Data.Text.Lazy
import qualified Data.Text.Lazy.Encoding
import qualified Elm
import qualified Elm.Servant
import qualified Golden.API
import Test.Tasty
import qualified Test.Tasty.Golden as Golden

main :: IO ()
main = defaultMain goldenTests

goldenTests :: TestTree
goldenTests = testGroup "golden" (goldenTestFor <$> apis)
  where
    apis = [Proxy :: Proxy Golden.API.API]

goldenTestFor :: Elm.Servant.HasElm api => Proxy api -> TestTree
goldenTestFor api = Golden.goldenVsString "API" "tests/Golden/API.elm" go
  where
    go =
      pure .
      Data.Text.Lazy.Encoding.encodeUtf8 .
      Data.Text.Lazy.fromStrict . Elm.printModule $
      Elm.Servant.elmTypes api
