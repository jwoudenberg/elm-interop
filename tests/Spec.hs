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

import qualified Data.Aeson
import Data.Text as Text
import qualified Elm.Wire

-- |
-- Small test of functionality in this library. Will be removed before release.
main :: IO ()
main
  -- putStrLn . Text.unpack . printTypes $ elmType (Proxy :: Proxy Foo)
 = do
  putStrLn . Text.unpack . Text.unlines $
    printTypes <$> elmTypes (Proxy :: Proxy Api)
  putStrLn . show . Data.Aeson.encode . Elm.Wire.ElmJson $
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
        Text
  | Baz
  deriving (Generic)

instance Elm Bar

data Unicorn
  deriving (Generic)

instance Elm Unicorn

type Api
   = "api" :> Capture "hi" Int32 :> ReqBody '[ ELM] Bar :> Post '[ ELM] Foo
