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
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import qualified Data.Scientific as Scientific
import Elm.Wire as Wire

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
    Tuple xs -> tuple xs
    Record xs -> record (Map.fromList xs)
    User name ->
      fromMaybe void $ do
        constructors <- Map.lookup name (unUserTypes userTypes)
        pure . sum' $ coderForType . (userTypes, ) <$> Map.fromList constructors
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
    , decode =
        \case
          Aeson.Number n -> MkInt <$> Scientific.toBoundedInteger n
          _ -> Nothing
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

record :: Map FieldName Coder -> Coder
record fields =
  Coder
    { encode =
        \case
          MkRecord xs ->
            let encodeField :: FieldName -> Wire.Value -> Maybe Aeson.Series
                encodeField name value = do
                  field <- Map.lookup name fields
                  Encoding.pair (Wire.unFieldName name) <$> encode field value
             in Encoding.pairs <$> Map.foldMapWithKey encodeField xs
          _ -> Nothing
    , decode =
        \case
          Aeson.Object xs ->
            let decodeField :: FieldName -> Coder -> Maybe (Map FieldName Value)
                decodeField name coder = do
                  value <- HashMap.lookup (Wire.unFieldName name) xs
                  Map.singleton name <$> decode coder value
             in MkRecord <$> Map.foldMapWithKey decodeField fields
          _ -> Nothing
    }

tuple :: [Coder] -> Coder
tuple params =
  Coder
    { encode =
        \case
          MkTuple xs
            | length xs == length params ->
              Encoding.list id <$> zipWithM ($) (encode <$> params) xs
          _ -> Nothing
    , decode =
        \case
          Aeson.Array xs
            | length xs == length params ->
              MkTuple <$> zipWithM ($) (decode <$> params) (toList xs)
          _ -> Nothing
    }

sum' :: Map Wire.ConstructorName Coder -> Coder
sum' constructors =
  Coder
    { encode =
        \case
          MkSum n x -> do
            constructor <- Map.lookup n constructors
            val <- encode constructor x
            let ctor = Encoding.text (Wire.unConstructorName n)
            pure . Encoding.pairs $
              (Encoding.pair "ctor" ctor) <> (Encoding.pair "val" val)
          _ -> Nothing
    , decode =
        \case
          Aeson.Object object -> do
            ctorValue <- HashMap.lookup "ctor" object
            ctor <-
              case ctorValue of
                Aeson.String text -> Just (Wire.ConstructorName text)
                _ -> Nothing
            val <- HashMap.lookup "val" object
            constructor <- Map.lookup ctor constructors
            decode constructor val
          _ -> Nothing
    }
