{-# LANGUAGE DeriveGeneric #-}

module Example where

import Data.Proxy (Proxy(Proxy))
import GHC.Generics (Generic, from)

import qualified Coder
import qualified HasElm

data Foo = Foo
  { jasper :: Int
  , daniel :: Int
  , bastiaan :: Int
  , mattias :: Int
  } deriving (Generic)

data Bar =
  Bar Int
      Int
      Int
      Int
  deriving (Generic)

foo :: Foo
foo = Foo 1 2 3 4

bar :: Bar
bar = Bar 1 2 3 4

main :: IO ()
main = do
  print $ Coder.encode (HasElm.coder Proxy) (from foo)
  print $ Coder.encode (HasElm.coder Proxy) (from bar)
