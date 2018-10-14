module Serialized
  ( Serialized(Serialized)
  ) where

import HasElm (HasElm)

import qualified Coder
import qualified Data.Aeson as Aeson
import qualified HasElm

newtype Serialized a =
  Serialized a

instance (HasElm a) => Aeson.ToJSON (Serialized a) where
  toJSON (Serialized x) = Coder.encode HasElm.coder x

instance (HasElm a) => Aeson.FromJSON (Serialized a) where
  parseJSON = fmap Serialized . Coder.decode HasElm.coder
