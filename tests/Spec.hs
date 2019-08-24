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
import qualified Wire
import qualified Wire.Json

-- |
-- Small test of functionality in this library. Will be removed before release.
main :: IO ()
main = do
  putStrLn . Text.unpack . printModule $ elmTypes (Proxy :: Proxy Api)
  let coder = Wire.Json.coderForType $ Wire.wireType (Proxy :: Proxy Foo)
  putStrLn . show . Wire.Json.encodeJson coder . Wire.toWire $
    Foo (12, "Hi", "Ho") () ["hello", "world"] Baz

data Foo = Foo
  { one :: (Int32, Text, Text)
  , two :: ()
  , three :: [Text]
  , four :: Bar
  } deriving (Generic)

instance Rep Foo

data Bar
  = Bar Void
        Int32
        Foo
  | Baz
  deriving (Generic)

instance Rep Bar

data Unicorn
  deriving (Generic)

instance Rep Unicorn

type Api
   = "api" :> Capture "hi" Int32 :> ReqBody '[ ELM] Bar :> Post '[ ELM] Foo
