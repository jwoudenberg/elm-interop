{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Elm.Wire
  ( WireType(..)
  , WireValueF(..)
  , WireValue
  , Elm(..)
  , Product(Product)
  , Field(Field)
  , ElmJson(ElmJson)
  ) where

import Data.Bifunctor (first)
import Data.Functor.Foldable (Fix(Fix), cata)
import Data.Int (Int32)
import Data.Proxy (Proxy(Proxy))
import Data.Text (Text)
import Data.Void (Void, absurd)
import GHC.Generics
import GHC.TypeLits (KnownSymbol, symbolVal)
import Numeric.Natural (Natural)

import qualified Data.Aeson
import qualified Data.Text as Text
import qualified Elm.Json as Json

data WireType
  = Int
  | Float
  | String
  | Unit
  | List WireType
  | Tuple (WireType, WireType, [WireType])
  | Sum Text
        [Product]

newtype Product =
  Product (Text, [Field])

newtype Field =
  Field (Text, WireType)

data WireValueF a
  = MkInt Int32
  | MkFloat Double
  | MkString Text
  | MkUnit
  | MkList [a]
  | MkTuple (a, a, [a])
  | MkSum NthConstructor
          [a]
  deriving (Functor)

type WireValue = Fix WireValueF

newtype NthConstructor =
  NthConstructor Natural
  deriving (Enum, Eq, Integral, Num, Ord, Real)

instance Data.Aeson.ToJSON WireValue where
  toJSON = Data.Aeson.toJSON . encode
  toEncoding = Data.Aeson.toEncoding . encode

encode :: WireValue -> Json.Encoded
encode =
  cata $ \case
    MkInt int -> Json.int int
    MkFloat float -> Json.float float
    MkString string -> Json.string string
    MkUnit -> Json.unit ()
    MkList xs -> Json.list xs
    MkTuple (x, y, zs) -> Json.list (x : y : zs)
    MkSum n xs -> Json.list [Json.int (fromIntegral n), Json.list (xs)]

-- |
-- Provide Aeson instances for any type that implements `Elm`.
newtype ElmJson a = ElmJson
  { unElmJson :: a
  }

instance Elm a => Data.Aeson.ToJSON (ElmJson a) where
  toJSON = Data.Aeson.toJSON . toWire . unElmJson
  toEncoding = Data.Aeson.toEncoding . toWire . unElmJson

-- |
-- Class of types that have a wire format representation. The class is named
-- 'Elm' because it is user facing, and so rather than naming it after the
-- exact thing it does (something like `HasWireFormat`), we name it after the
-- thing the wire formats it produces are used for.
class Elm (a :: *) where
  wireType :: Proxy a -> WireType
  toWire :: a -> WireValue
  fromWire :: WireValue -> Maybe a
  -- Default Generics-based implementations.
  default wireType :: (ElmG (Rep a)) =>
    Proxy a -> WireType
  wireType _ = wireTypeG (Proxy @(Rep a))
  default toWire :: (Generic a, ElmG (Rep a)) =>
    a -> WireValue
  toWire = toWireG . from
  default fromWire :: (Generic a, ElmG (Rep a)) =>
    WireValue -> Maybe a
  fromWire = fmap to . fromWireG

instance Elm Int32 where
  wireType _ = Int
  toWire = Fix . MkInt
  fromWire (Fix (MkInt int)) = Just int
  fromWire _ = Nothing

instance Elm Double where
  wireType _ = Float
  toWire = Fix . MkFloat
  fromWire (Fix (MkFloat float)) = Just float
  fromWire _ = Nothing

instance Elm Text where
  wireType _ = String
  toWire = Fix . MkString
  fromWire (Fix (MkString string)) = Just string
  fromWire _ = Nothing

instance Elm () where
  wireType _ = Unit
  toWire () = Fix MkUnit
  fromWire _ = pure ()

instance Elm Void where
  wireType _ = Sum "" []
  toWire = absurd
  fromWire _ = Nothing

instance Elm a => Elm [a] where
  wireType _ = List (wireType (Proxy @a))
  toWire = Fix . MkList . fmap toWire
  fromWire (Fix (MkList xs)) = traverse fromWire xs
  fromWire _ = Nothing

-- Instances for tuples.
-- The 7-tuple is the largest one that has a Generics instance, so we'll support
-- up to that number here too.
instance (Elm a, Elm b) => Elm (a, b) where
  wireType _ = Tuple (wireType (Proxy @a), wireType (Proxy @b), [])
  toWire (a, b) = Fix $ MkTuple (toWire a, toWire b, [])
  fromWire (Fix (MkTuple (a, b, []))) = (,) <$> fromWire a <*> fromWire b
  fromWire _ = Nothing

instance (Elm a, Elm b, Elm c) => Elm (a, b, c) where
  wireType _ =
    Tuple (wireType (Proxy @a), wireType (Proxy @b), [wireType (Proxy @c)])
  toWire (a, b, c) = Fix $ MkTuple (toWire a, toWire b, [toWire c])
  fromWire (Fix (MkTuple (a, b, [c]))) =
    (,,) <$> fromWire a <*> fromWire b <*> fromWire c
  fromWire _ = Nothing

instance (Elm a, Elm b, Elm c, Elm d) => Elm (a, b, c, d) where
  wireType _ =
    Tuple
      ( wireType (Proxy @a)
      , wireType (Proxy @b)
      , [wireType (Proxy @c), wireType (Proxy @d)])
  toWire (a, b, c, d) = Fix $ MkTuple (toWire a, toWire b, [toWire c, toWire d])
  fromWire (Fix (MkTuple (a, b, [c, d]))) =
    (,,,) <$> fromWire a <*> fromWire b <*> fromWire c <*> fromWire d
  fromWire _ = Nothing

instance (Elm a, Elm b, Elm c, Elm d, Elm e) => Elm (a, b, c, d, e) where
  wireType _ =
    Tuple
      ( wireType (Proxy @a)
      , wireType (Proxy @b)
      , [wireType (Proxy @c), wireType (Proxy @d), wireType (Proxy @e)])
  toWire (a, b, c, d, e) =
    Fix $ MkTuple (toWire a, toWire b, [toWire c, toWire d, toWire e])
  fromWire (Fix (MkTuple (a, b, [c, d, e]))) =
    (,,,,) <$> fromWire a <*> fromWire b <*> fromWire c <*> fromWire d <*>
    fromWire e
  fromWire _ = Nothing

instance (Elm a, Elm b, Elm c, Elm d, Elm e, Elm f) =>
         Elm (a, b, c, d, e, f) where
  wireType _ =
    Tuple
      ( wireType (Proxy @a)
      , wireType (Proxy @b)
      , [ wireType (Proxy @c)
        , wireType (Proxy @d)
        , wireType (Proxy @e)
        , wireType (Proxy @f)
        ])
  toWire (a, b, c, d, e, f) =
    Fix $ MkTuple (toWire a, toWire b, [toWire c, toWire d, toWire e, toWire f])
  fromWire (Fix (MkTuple (a, b, [c, d, e, f]))) =
    (,,,,,) <$> fromWire a <*> fromWire b <*> fromWire c <*> fromWire d <*>
    fromWire e <*>
    fromWire f
  fromWire _ = Nothing

instance (Elm a, Elm b, Elm c, Elm d, Elm e, Elm f, Elm g) =>
         Elm (a, b, c, d, e, f, g) where
  wireType _ =
    Tuple
      ( wireType (Proxy @a)
      , wireType (Proxy @b)
      , [ wireType (Proxy @c)
        , wireType (Proxy @d)
        , wireType (Proxy @e)
        , wireType (Proxy @f)
        , wireType (Proxy @g)
        ])
  toWire (a, b, c, d, e, f, g) =
    Fix $
    MkTuple
      (toWire a, toWire b, [toWire c, toWire d, toWire e, toWire f, toWire g])
  fromWire (Fix (MkTuple (a, b, [c, d, e, f, g]))) =
    (,,,,,,) <$> fromWire a <*> fromWire b <*> fromWire c <*> fromWire d <*>
    fromWire e <*>
    fromWire f <*>
    fromWire g
  fromWire _ = Nothing

-- |
-- Helper class for constructing write types from generic representations of
-- types.
class ElmG (f :: * -> *) where
  wireTypeG :: Proxy f -> WireType
  toWireG :: f p -> WireValue
  fromWireG :: WireValue -> Maybe (f p)

instance (Elm c) => ElmG (K1 i c) where
  wireTypeG _ = wireType (Proxy @c)
  toWireG = toWire . unK1
  fromWireG = fmap K1 . fromWire

instance (HasName m, SumsG f) => ElmG (M1 D m f) where
  wireTypeG _ = Sum (name (Proxy @m)) (sumsG (Proxy @f))
  toWireG = Fix . uncurry MkSum . toSumsG . unM1
  fromWireG (Fix (MkSum n x)) = fmap M1 $ fromSumsG n x
  fromWireG _ = Nothing

-- |
-- Helper class for constructing sums of types.
class SumsG (f :: * -> *) where
  sumsG :: Proxy f -> [Product]
  toSumsG :: f p -> (NthConstructor, [WireValue])
  fromSumsG :: NthConstructor -> [WireValue] -> Maybe (f p)

instance (SumsG f, SumsG g) => SumsG (f :+: g) where
  sumsG _ = sumsG (Proxy @f) <> sumsG (Proxy @g)
  toSumsG (L1 x) = toSumsG x
  toSumsG (R1 x) = first (+ 1) $ toSumsG x
  fromSumsG 0 x = fmap L1 (fromSumsG 0 x)
  fromSumsG n x = fmap R1 (fromSumsG (n - 1) x)

instance (HasName m, ProductG f) => SumsG (M1 C m f) where
  sumsG _ = [Product (name (Proxy @m), productG (Proxy @f))]
  toSumsG = (0, ) . toProductG . unM1
  fromSumsG 0 x = fmap M1 (fromProductG x)
  fromSumsG _ _ = Nothing -- ^ We picked a constructor that doesn't exist.

instance SumsG V1 where
  sumsG _ = []
  toSumsG = \case {}
  fromSumsG _ _ = Nothing

-- |
-- Helper class for constructing products.
class ProductG (f :: * -> *) where
  productG :: Proxy f -> [Field]
  toProductG :: f p -> [WireValue]
  fromProductG :: [WireValue] -> Maybe (f p)

instance (ProductG f, ProductG g) => ProductG (f :*: g) where
  productG _ = productG (Proxy @f) <> productG (Proxy @g)
  toProductG (x :*: y) = toProductG x <> toProductG y
  fromProductG z = (:*:) <$> fromProductG x <*> fromProductG y
    where
      (x, y) = splitAt (length . productG $ Proxy @f) z

instance (HasName m, ElmG f) => ProductG (M1 S m f) where
  productG _ = [Field (name (Proxy @m), wireTypeG (Proxy @f))]
  toProductG = pure . toWireG . unM1
  fromProductG [x] = M1 <$> fromWireG x
  fromProductG _ = Nothing -- ^ We got the wrong number of constructors.

instance ProductG U1 where
  productG _ = []
  toProductG U1 = []
  fromProductG [] = Just U1
  fromProductG _ = Nothing

-- |
-- Helper class for extracting the type-level name of a `Meta` kind.
-- Used to recover names of types, constructors, and fields.
class HasName (a :: Meta) where
  name :: Proxy a -> Text

instance (KnownSymbol n) => HasName ('MetaData n m p nt) where
  name _ = Text.pack $ symbolVal (Proxy @n)

instance (KnownSymbol n) => HasName ('MetaCons n f s) where
  name _ = Text.pack $ symbolVal (Proxy @n)

instance (KnownSymbol n) => HasName ('MetaSel ('Just n) m p nt) where
  name _ = Text.pack $ symbolVal (Proxy @n)

instance HasName ('MetaSel 'Nothing m p nt) where
  name _ = ""
