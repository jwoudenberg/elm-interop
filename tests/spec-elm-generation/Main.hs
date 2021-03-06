{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Main
  ( main,
  )
where

import Data.Proxy (Proxy (Proxy))
import qualified Data.Text as T
import qualified Data.Text.Lazy
import qualified Data.Text.Lazy.Encoding
import qualified Examples.MultipleEndpoints
import qualified Examples.MutuallyRecursive
import qualified Examples.PathParams
import qualified Examples.QueryParams
import qualified Examples.Record
import qualified Examples.Recursive
import qualified Examples.RequestBody
import qualified Examples.RequestHeaders
import qualified Examples.Roundtrip
import qualified Examples.Void
import qualified Servant.Interop
import qualified Servant.Interop.Elm as Elm
import Test.Tasty
import qualified Test.Tasty.Golden as Golden
import qualified Test.Tasty.Program as Program

main :: IO ()
main = defaultMain tests

data Example where
  Example :: Servant.Interop.HasWireFormat api => String -> Proxy api -> Example

examples :: [Example]
examples =
  [ Example "Record" (Proxy :: Proxy Examples.Record.API),
    Example "Recursive" (Proxy :: Proxy Examples.Recursive.API),
    Example "MutuallyRecursive" (Proxy :: Proxy Examples.MutuallyRecursive.API),
    Example "RequestBody" (Proxy :: Proxy Examples.RequestBody.API),
    Example "Void" (Proxy :: Proxy Examples.Void.API),
    Example "QueryParams" (Proxy :: Proxy Examples.QueryParams.API),
    Example "RequestHeaders" (Proxy :: Proxy Examples.RequestHeaders.API),
    Example "PathParams" (Proxy :: Proxy Examples.PathParams.API),
    Example "MultipleEndpoints" (Proxy :: Proxy Examples.MultipleEndpoints.API),
    Example "Roundtrip" (Proxy :: Proxy Examples.Roundtrip.API)
  ]

tests :: TestTree
tests =
  testGroup
    "Elm code generation"
    [ goldenTests,
      elmMakeTests,
      elmFormatTests
    ]

goldenTests :: TestTree
goldenTests =
  testGroup "golden" $ goldenTestFor <$> examples

elmMakeTests :: TestTree
elmMakeTests =
  testGroup "elm make <file>" $ elmMakeTestFor <$> examples

elmFormatTests :: TestTree
elmFormatTests =
  testGroup "elm-format --validate <file>" $ elmFormatTestFor <$> examples

goldenTestFor :: Example -> TestTree
goldenTestFor (Example name api) =
  Golden.goldenVsString name ("tests/elm-generated/" <> name <> ".elm") go
  where
    go =
      pure . Data.Text.Lazy.Encoding.encodeUtf8 . Data.Text.Lazy.fromStrict $
        Elm.printModule (Elm.Options (T.pack name)) api

elmMakeTestFor :: Example -> TestTree
elmMakeTestFor (Example name _) =
  Program.testProgram
    file
    "elm"
    ["make", file]
    (Just "tests/elm-test-app")
  where
    file = "../elm-generated/" <> name <> ".elm"

elmFormatTestFor :: Example -> TestTree
elmFormatTestFor (Example name _) =
  Program.testProgram
    file
    "elm-format"
    ["--validate", file]
    (Just "tests/elm-test-app")
  where
    file = "../elm-generated/" <> name <> ".elm"
