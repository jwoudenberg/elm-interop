{-# LANGUAGE DeriveGeneric #-}

module Spec
  ( main
  ) where

import Data.Int (Int32)
import Data.Proxy (Proxy(Proxy))
import Data.Void (Void)
import Elm
import GHC.Generics (Generic)

import Data.Text as Text

-- |
-- Small test of functionality in this library. Will be removed before release.
main :: IO ()
main = do
  putStrLn . Text.unpack . printTypes $ elmType (Proxy :: Proxy Foo)

data Foo = Foo
  { one :: (Int32, Text, Text)
  , two :: ()
  , three :: [Text]
  , four :: Bar
  , five :: Unicorn
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
