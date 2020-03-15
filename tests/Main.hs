{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Main
  ( main,
  )
where

import Control.Concurrent.MVar as MVar
import Control.Monad.IO.Class (liftIO)
import Data.Function ((&))
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
import qualified Network.Wai.Handler.Warp as Warp
import Servant ((:<|>) ((:<|>)))
import qualified Servant
import qualified Servant.Interop
import qualified Servant.Interop.Elm as Elm
import qualified Servant.Server
import qualified Servant.Server.StaticFiles as StaticFiles
import qualified System.Directory as Directory
import qualified System.IO
import qualified System.Process.Typed as Process
import qualified System.Timeout
import Test.Tasty
import qualified Test.Tasty.Golden as Golden
import qualified Test.Tasty.HUnit as HUnit
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
    "servant-interop"
    [ goldenTests,
      elmMakeTests,
      elmFormatTests,
      roundtripTests
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
  Golden.goldenVsString name ("tests/reference/" <> name <> ".elm") go
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
    file = "../reference/" <> name <> ".elm"

elmFormatTestFor :: Example -> TestTree
elmFormatTestFor (Example name _) =
  Program.testProgram
    file
    "elm-format"
    ["--validate", file]
    (Just "tests/elm-test-app")
  where
    file = "../reference/" <> name <> ".elm"

roundtripTests :: TestTree
roundtripTests =
  HUnit.testCase "roundtrip" $ do
    liftIO compileElmTestApp
    servedValue <- liftIO $ MVar.newEmptyMVar
    receivedValue <- liftIO $ MVar.newEmptyMVar
    let settings = Examples.Roundtrip.Settings servedValue receivedValue
    Warp.testWithApplication (pure (app settings)) $ \port -> do
      let location = "http://localhost:" <> show port <> "/index.html"
      MVar.putMVar servedValue (Examples.Roundtrip.Value 42)
      Process.withProcessTerm (chromeProc location) $ \chrome -> do
        let _stdin = Process.getStdin chrome
        -- System.IO.hPutStrLn stdin ("location.assign('" <> location <> "');")
        -- System.IO.hFlush stdin
        res <- System.Timeout.timeout 1000000 $ MVar.takeMVar receivedValue
        case res of
          Nothing -> fail "No response from Elm app within aloted time."
          Just val -> print val

chromeProc :: String -> Process.ProcessConfig System.IO.Handle () ()
chromeProc location =
  Process.proc
    "/Applications/Google Chrome.app/Contents/MacOS/Google Chrome"
    [ "--headless",
      -- Setting these flags will suppress what would otherwise be console
      -- errors.
      "--user-data-dir=/tmp/chrome/logs",
      "--crash-dumps-dir=/tmp/chrome/crash-dumps",
      -- The Elm app will throw an error if it encounters anything unexpected.
      -- We'd like to see these in the terminal output of the test.
      "--enable-logging=stderr",
      "--v=1",
      -- We need to keep Chrome running beyond the first page load, as the
      -- roundtrip Elm application is going two make multiple sequential HTTP
      -- requests.
      "--remote-debugging-port=22233",
      location
    ]
    & Process.setStdin Process.createPipe

compileElmTestApp :: IO ()
compileElmTestApp =
  Directory.withCurrentDirectory "tests/elm-test-app" $ do
    Process.runProcess_ $
      Process.proc "elm" ["make", "src/Main.elm", "--output", "index.html"]

app :: Examples.Roundtrip.Settings -> Servant.Server.Application
app settings =
  Servant.Server.serve
    (Proxy :: Proxy (Examples.Roundtrip.API :<|> Servant.Raw))
    ( Examples.Roundtrip.server settings
        :<|> StaticFiles.serveDirectoryWebApp "tests/elm-test-app"
    )
