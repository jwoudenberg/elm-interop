{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Spec
  ( main
  ) where

import Data.Int (Int32)
import Data.Proxy (Proxy(Proxy))
import Data.Void (Void)
import Elm
import Elm.Servant
import GHC.Generics (Generic)
import Servant.API

import Data.Text as Text
import qualified Elm.Wire
import qualified Elm.Wire.Json

-- |
-- Small test of functionality in this library. Will be removed before release.
main :: IO ()
main = do
  putStrLn . Text.unpack . printModule $ elmTypes (Proxy :: Proxy Api)
  let coder =
        Elm.Wire.Json.coderForType $ Elm.Wire.wireType (Proxy :: Proxy Foo)
  putStrLn . show . Elm.Wire.Json.encodeJson coder . Elm.Wire.toWire $
    Foo (12, "Hi", "Ho") () ["hello", "world"] Baz

data Foo = Foo
  { one :: (Int32, Text, Text)
  , two :: ()
  , three :: [Text]
  , four :: Bar
  } deriving (Generic)

instance Elm Foo

data Bar
  = Bar Void
        Int32
        Foo
  | Baz
  deriving (Generic)

instance Elm Bar

data Unicorn
  deriving (Generic)

instance Elm Unicorn

type Api
   = "api" :> Capture "hi" Int32 :> ReqBody '[ ELM] Bar :> Post '[ ELM] Foo
