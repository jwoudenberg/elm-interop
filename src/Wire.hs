{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
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

module Wire
  ( WireType(..)
  , WireValue(..)
  , Elm(..)
  , Product(Product)
  , Field(Field)
  ) where

import Data.Bifunctor (first)
import Data.Int (Int32)
import Data.Proxy (Proxy(Proxy))
import Data.Text (Text)
import GHC.Generics
import GHC.TypeLits (KnownSymbol, symbolVal)
import Numeric.Natural (Natural)

import qualified Data.Text as Text

data WireType
  = Int
  | Float
  | String
  | Sum Text
        [Product]

newtype Product =
  Product (Text, [Field])

newtype Field =
  Field (Text, WireType)

data WireValue
  = MkInt Int32
  | MkFloat Double
  | MkString Text
  | MkSum NthConstructor
          [WireValue]

newtype NthConstructor =
  NthConstructor Natural
  deriving (Eq, Num)

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
  toWire = MkInt
  fromWire (MkInt int) = Just int
  fromWire _ = Nothing

instance Elm Double where
  wireType _ = Float
  toWire = MkFloat
  fromWire (MkFloat float) = Just float
  fromWire _ = Nothing

instance Elm Text where
  wireType _ = String
  toWire = MkString
  fromWire (MkString string) = Just string
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
  toWireG = uncurry MkSum . toSumsG . unM1
  fromWireG (MkSum n x) = fmap M1 $ fromSumG n x
  fromWireG _ = Nothing

-- |
-- Helper class for constructing sums of types.
class SumsG (f :: * -> *) where
  sumsG :: Proxy f -> [Product]
  toSumsG :: f p -> (NthConstructor, [WireValue])
  fromSumG :: NthConstructor -> [WireValue] -> Maybe (f p)

instance (SumsG f, SumsG g) => SumsG (f :+: g) where
  sumsG _ = sumsG (Proxy @f) <> sumsG (Proxy @g)
  toSumsG (L1 x) = toSumsG x
  toSumsG (R1 x) = first (+ 1) $ toSumsG x
  fromSumG 0 x = fmap L1 (fromSumG 0 x)
  fromSumG n x = fmap R1 (fromSumG (n - 1) x)

instance (HasName m, ProductG f) => SumsG (M1 C m f) where
  sumsG _ = [Product (name (Proxy @m), productG (Proxy @f))]
  toSumsG = (0, ) . toProductG . unM1
  fromSumG 0 x = fmap M1 (fromProductG x)
  fromSumG _ _ = Nothing -- ^ We picked a constructor that doesn't exist.

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
