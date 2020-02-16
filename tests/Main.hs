module Main
  ( main,
  )
where

import Data.Proxy (Proxy (Proxy))
import qualified Data.Text.Lazy
import qualified Data.Text.Lazy.Encoding
import qualified Golden.MutuallyRecursive
import qualified Golden.Record
import qualified Golden.Recursive
import qualified Golden.RequestBody
import qualified Golden.Void
import qualified Servant.Interop
import qualified Servant.Interop.Elm as Elm
import Test.Tasty
import qualified Test.Tasty.Golden as Golden

main :: IO ()
main = defaultMain goldenTests

goldenTests :: TestTree
goldenTests =
  testGroup
    "golden"
    [ goldenTestFor "Record" (Proxy :: Proxy Golden.Record.API),
      goldenTestFor "Recursive" (Proxy :: Proxy Golden.Recursive.API),
      goldenTestFor "MutuallyRecursive" (Proxy :: Proxy Golden.MutuallyRecursive.API),
      goldenTestFor "RequestBody" (Proxy :: Proxy Golden.RequestBody.API),
      goldenTestFor "Void" (Proxy :: Proxy Golden.Void.API)
    ]

goldenTestFor ::
  Servant.Interop.HasWireFormat api => String -> Proxy api -> TestTree
goldenTestFor name api =
  Golden.goldenVsString name ("tests/Golden/" <> name <> ".elm") go
  where
    go =
      pure . Data.Text.Lazy.Encoding.encodeUtf8 . Data.Text.Lazy.fromStrict $
        Elm.printModule api
