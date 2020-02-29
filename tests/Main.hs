{-# LANGUAGE GADTs #-}
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

data Example where
  Example :: Servant.Interop.HasWireFormat api => String -> Proxy api -> Example

examples :: [Example]
examples =
  [ Example "Record" (Proxy :: Proxy Examples.Record.API),
    Example "Recursive" (Proxy :: Proxy Examples.Recursive.API),
    Example "MutuallyRecursive" (Proxy :: Proxy Examples.MutuallyRecursive.API),
    Example "RequestBody" (Proxy :: Proxy Examples.RequestBody.API),
    Example "Void" (Proxy :: Proxy Examples.Void.API),
    Example "ComplexApi" (Proxy :: Proxy Examples.ComplexApi.API)
  ]

goldenTests :: TestTree
goldenTests =
  testGroup "golden" $ goldenTestFor <$> examples

goldenTestFor ::
  Example -> TestTree
goldenTestFor (Example name api) =
  Golden.goldenVsString name (generatedElmFor name) go
  where
    go =
      pure . Data.Text.Lazy.Encoding.encodeUtf8 . Data.Text.Lazy.fromStrict $
        Elm.printModule (Elm.Options "http://example.com" "Generated") api

generatedElmFor :: String -> FilePath
generatedElmFor name = "tests/reference/" <> name <> ".elm"
