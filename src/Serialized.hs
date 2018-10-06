module Serialized
  ( Serialized(Serialized)
  ) where

import HasElm (HasElm(hasElm))

import qualified Coder
import qualified Data.Aeson as Aeson
import qualified ToElm

newtype Serialized a =
  Serialized a

instance (HasElm a) => Aeson.ToJSON (Serialized a) where
  toJSON (Serialized x) = Coder.encode (ToElm.coder hasElm) x

instance (HasElm a) => Aeson.FromJSON (Serialized a) where
  parseJSON = fmap Serialized . Coder.decode (ToElm.coder hasElm)
