{-# LANGUAGE OverloadedStrings #-}

module Main
  ( main,
  )
where

import Data.Proxy (Proxy (Proxy))
import qualified Data.Text.Lazy
import qualified Data.Text.Lazy.Encoding
import qualified Examples.ComplexApi
import qualified Examples.MutuallyRecursive
import qualified Examples.Record
import qualified Examples.Recursive
import qualified Examples.RequestBody
import qualified Examples.Void
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
    [ goldenTestFor "Record" (Proxy :: Proxy Examples.Record.API),
      goldenTestFor "Recursive" (Proxy :: Proxy Examples.Recursive.API),
      goldenTestFor "MutuallyRecursive" (Proxy :: Proxy Examples.MutuallyRecursive.API),
      goldenTestFor "RequestBody" (Proxy :: Proxy Examples.RequestBody.API),
      goldenTestFor "Void" (Proxy :: Proxy Examples.Void.API),
      goldenTestFor "ComplexApi" (Proxy :: Proxy Examples.ComplexApi.API)
    ]

goldenTestFor ::
  Servant.Interop.HasWireFormat api => String -> Proxy api -> TestTree
goldenTestFor name api =
  Golden.goldenVsString name ("tests/Golden/reference/" <> name <> ".elm") go
  where
    go =
      pure . Data.Text.Lazy.Encoding.encodeUtf8 . Data.Text.Lazy.fromStrict $
        Elm.printModule (Elm.Options "http://example.com" "Generated") api
