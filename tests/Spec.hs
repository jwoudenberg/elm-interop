{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Spec
  ( main
  ) where

import Data.Int (Int32)
import Data.Proxy (Proxy(Proxy))
import Data.Void (Void)
import Elm
import GHC.Generics (Generic)

import qualified Data.Aeson
import Data.Text as Text
import qualified Elm.Wire

-- |
-- Small test of functionality in this library. Will be removed before release.
main :: IO ()
main = do
  putStrLn . Text.unpack . printTypes $ elmType (Proxy :: Proxy Foo)
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
