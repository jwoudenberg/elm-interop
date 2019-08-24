{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Elm.Wire.Json
  ( Coder
  , coderForType
  , encodeJson
  , decodeJson
  ) where

import Control.Monad (zipWithM)
import qualified Data.Aeson.Encoding as Encoding
import qualified Data.Aeson.Parser as Parser
import qualified Data.Aeson.Types as Aeson
import Data.ByteString.Lazy (ByteString)
import Data.Foldable (toList)
import Data.Functor.Foldable (cata)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import qualified Data.Scientific as Scientific
import Elm.Wire as Wire
import Safe (atMay)

data Coder = Coder
  { encode :: Wire.Value -> Maybe Aeson.Encoding
  , decode :: Aeson.Value -> Maybe Wire.Value
  }

encodeJson :: Coder -> Wire.Value -> Maybe ByteString
encodeJson coder = fmap Encoding.encodingToLazyByteString . encode coder

decodeJson :: Coder -> ByteString -> Maybe Wire.Value
decodeJson coder =
  Parser.decodeWith
    Parser.value
    (maybe (fail "JSON has a different structure than expected.") pure .
     decode coder)

coderForType :: (UserTypes, Wire.PrimitiveType) -> Coder
coderForType (userTypes, type_) =
  flip cata type_ $ \case
    Product xs -> product' (snd <$> xs)
    User name ->
      fromMaybe void $ do
        constructors <- Map.lookup name (unUserTypes userTypes)
        pure . sum' $ coderForType . (userTypes, ) . snd <$> constructors
    Void -> void
    List el -> list el
    Int -> int
    Float -> float
    String -> string

void :: Coder
void = Coder {encode = const Nothing, decode = const Nothing}

int :: Coder
int =
  Coder
    { encode =
        \case
          MkInt int32 -> Just (Encoding.int32 int32)
          _ -> Nothing
    , decode = fmap MkInt . parseInt
    }

float :: Coder
float =
  Coder
    { encode =
        \case
          MkFloat double -> Just (Encoding.double double)
          _ -> Nothing
    , decode =
        \case
          Aeson.Number n -> Just . MkFloat $ Scientific.toRealFloat n
          _ -> Nothing
    }

string :: Coder
string =
  Coder
    { encode =
        \case
          MkString text -> Just (Encoding.text text)
          _ -> Nothing
    , decode =
        \case
          Aeson.String text -> Just (MkString text)
          _ -> Nothing
    }

list :: Coder -> Coder
list el =
  Coder
    { encode =
        \case
          MkList xs -> Encoding.list id <$> traverse (encode el) xs
          _ -> Nothing
    , decode =
        \case
          Aeson.Array xs -> MkList <$> traverse (decode el) (toList xs)
          _ -> Nothing
    }

product' :: [Coder] -> Coder
product' fields =
  Coder
    { encode =
        \case
          MkProduct xs
            | length xs == length fields ->
              Encoding.list id <$> zipWithM ($) (encode <$> fields) xs
          _ -> Nothing
    , decode =
        \case
          Aeson.Array xs
            | length xs == length fields ->
              MkProduct <$> zipWithM ($) (decode <$> fields) (toList xs)
          _ -> Nothing
    }

sum' :: [Coder] -> Coder
sum' constructors =
  Coder
    { encode =
        \case
          MkSum n x -> do
            constructor <- atMay constructors (fromIntegral n)
            value <- encode constructor x
            pure . Encoding.pairs $
              (Encoding.pair "ctor" (Encoding.int32 (fromIntegral n))) <>
              (Encoding.pair "val" value)
          _ -> Nothing
    , decode =
        \case
          Aeson.Object object -> do
            n <- parseInt =<< HashMap.lookup "ctor" object
            value <- HashMap.lookup "val" object
            constructor <- atMay constructors n
            decode constructor value
          _ -> Nothing
    }

parseInt :: (Integral n, Bounded n) => Aeson.Value -> Maybe n
parseInt =
  \case
    Aeson.Number n -> Scientific.toBoundedInteger n
    _ -> Nothing
