{-# LANGUAGE GADTs #-}

module Elm.Json
  ( Encoded
  , int
  , float
  , string
  , unit
  , list
  , object
  ) where

-- |
-- Helpers for Elm-style precise JSON construction.
--
-- Aeson is great at generating default JSON encoders and decoders for types.
-- Sometimes we need to precisely define how a particular value is encoded
-- though, such as in this case, where it is essential Haskell and Elm encoders
-- and decoders produce and consume the same data. In this case you want to make
-- encoders rely solely on manually written encoding logic, and don't
-- accidentally incorporate logic from some `ToJSON` instance. That can be hard
-- to achieve using Aeson.
--
-- Using this module does not require any `ToJSON` instance to be created and
-- so prevents these accidental type class instances from slipping in.
import Data.Aeson hiding (object)
import Data.Int (Int32)
import Data.Text (Text)
import GHC.Exts (fromList)

import qualified Data.Aeson

int :: Int32 -> Encoded
int = Encoded toEncoding toJSON

float :: Double -> Encoded
float = Encoded toEncoding toJSON

string :: Text -> Encoded
string = Encoded toEncoding toJSON

unit :: () -> Encoded
unit = Encoded toEncoding toJSON

list :: [Encoded] -> Encoded
list = Encoded foldable (Array . fromList . fmap toJSON)

object :: [(Text, Encoded)] -> Encoded
object =
  Encoded
    (pairs . foldMap (uncurry (.=)))
    (Data.Aeson.object . (fmap . fmap) toJSON)

data Encoded where
  Encoded
    :: (a -> Data.Aeson.Encoding) -> (a -> Data.Aeson.Value) -> a -> Encoded

instance ToJSON Encoded where
  toJSON (Encoded _ e x) = e x
  toEncoding (Encoded e _ x) = e x
