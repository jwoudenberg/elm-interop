{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
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
  ( PrimitiveType
  , PrimitiveTypeF(..)
  , Value(..)
  , UserTypes(..)
  , Elm
  , wireType
  , ElmJson(ElmJson)
  ) where

import Control.Monad (unless)
import Control.Monad.Reader (Reader, ask, local, runReader)
import Control.Monad.Writer.Strict (WriterT, runWriterT, tell)
import Data.Bifunctor (first)
import Data.Functor.Foldable (Fix(Fix))
import Data.Int (Int32)
import Data.Map.Strict (Map)
import Data.Proxy (Proxy(Proxy))
import Data.Set (Set)
import Data.Text (Text)
import Data.Tuple (swap)
import Data.Void (Void, absurd)
import GHC.Generics
import GHC.TypeLits (KnownSymbol, symbolVal)
import Numeric.Natural (Natural)

import qualified Data.Aeson
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as Text

-- |
-- A type to describe the shape of data flowing between the front- and backend.
--
-- We generate Elm types based on descriptions of types expressed in `PrimitiveType`
-- so there's enough information there for that, but otherwise the intention is
-- for this type to be as small as possible, which is to say having the least
-- amount of constructors.
--
-- Having a small type here contributes to simplifying other parts of the code:
-- - Using `GHC.Generics` we produce `PrimitiveType` representations of all Haskell
--   types. Generics code by its nature is spread out across a large number of
--   type class instances. The more high end-user concepts like 'records' or
--   'tuple we add to this type the more we have to perform the construction of
--   those higher level constructs in our generics code. By keeping the type
--   smaller, and closer to the way `GHC.Generics` represents types we can deal
--   with the generics first, and with end-user concepts separately.
-- - Because our Elm and Haskell code communicate using these types, both need
--   to have compatible implementations for encoding/decoding it. The smaller
--   the type the fewer opportunities there are for these implementations to be
--   misaligned.
data PrimitiveTypeF a
  = Product [(Text, a)]
  -- ^ A product type, like a tuple or a record.
  --
  --     Product [(Text             , a)]
  --               ^^^^               ^
  --               Field name (       Field type
  --               "" for tuples")
  --
  | User Text
  -- ^ A reference to a non-primitive type, for example a user defined types.
  | Void
  -- ^ A type without any values. Goes by `Never` in Elm.
  | List a
  -- ^ Not really a primitive and we could scrap this, treating lists as just
  -- another data type using `Nil` and `Cons` constructors. That would be a pain
  -- to work with though and probably not terribly efficient over the wire, so
  -- we're making an exception.
  | Int
  | Float
  | String
  deriving (Functor)

type PrimitiveType = Fix PrimitiveTypeF

-- |
-- Everything that isn't a primitive type is a user type. We always keep their
-- definitions together in a map to avoid duplicates.
--
-- A user type is a Haskell sum type or Elm custom type. It can have multiple
-- constructors, each with one or more parameters.
newtype UserTypes = UserTypes
  { unUserTypes :: Map Text [(Text, PrimitiveType)]
  } deriving (Semigroup, Monoid)

-- |
-- The `PrimitiveType` describes the types of the things going over the wire between
-- Haskell and Elm. This `Value` type describes the values of those types
-- we're going to encode and decode.
--
-- As you can see the `Value` constructors make no mention of field names or
-- constructor names. Instead we use the order of fields in products and
-- constructors in sums to indicate which value belongs where. This means you
-- need the `PrimitiveType` to be able to decode a `Value`, and also that the
-- data going over the wire will be hard for humans to grok.
--
-- There's reasons we do it this way regardless:
-- - The amount of data we're sending over the wire is smaller this way (Though
--   arguably gzipping would take care of a lot of that anyway).
-- - It gives us freedom to change aspects of the generated Elm code (like the
--   names of types and constructors) without affecting encoding and decoding
--   logic. This is important because we need flexibility in renaming to prevent
--   naming collisions in generated code.
data Value
  = MkInt Int32
  | MkFloat Double
  | MkString Text
  | MkList [Value]
  | MkProduct [Value]
  | MkSum NthConstructor
          Value
  deriving (Generic)

instance Data.Aeson.ToJSON Value

instance Data.Aeson.FromJSON Value

newtype NthConstructor =
  NthConstructor Natural
  deriving ( Data.Aeson.ToJSON
           , Data.Aeson.FromJSON
           , Enum
           , Eq
           , Integral
           , Num
           , Ord
           , Real
           )

-- |
-- Provide Aeson instances for any type that implements `Elm`.
newtype ElmJson a = ElmJson
  { unElmJson :: a
  }

instance Elm a => Data.Aeson.ToJSON (ElmJson a) where
  toJSON = Data.Aeson.toJSON . toWire . unElmJson
  toEncoding = Data.Aeson.toEncoding . toWire . unElmJson

wireType :: Elm a => Proxy a -> (UserTypes, PrimitiveType)
wireType = swap . flip runReader mempty . runWriterT . wireType'

-- |
-- Class of types that have a wire format representation. The class is named
-- 'Elm' because it is user facing, and so rather than naming it after the
-- exact thing it does (something like `HasWireFormat`), we name it after the
-- thing the wire formats it produces are used for.
class Elm (a :: *) where
  wireType' :: Proxy a -> Builder PrimitiveType
  toWire :: a -> Value
  fromWire :: Value -> Maybe a
  -- Default Generics-based implementations.
  default wireType' :: (ElmG (Rep a)) =>
    Proxy a -> Builder PrimitiveType
  wireType' _ = wireTypeG (Proxy @(Rep a))
  default toWire :: (Generic a, ElmG (Rep a)) =>
    a -> Value
  toWire = toWireG . from
  default fromWire :: (Generic a, ElmG (Rep a)) =>
    Value -> Maybe a
  fromWire = fmap to . fromWireG

type Builder a = WriterT UserTypes (Reader (Set Text)) a

instance Elm Int32 where
  wireType' _ = pure $ Fix Int
  toWire = MkInt
  fromWire (MkInt int) = Just int
  fromWire _ = Nothing

instance Elm Double where
  wireType' _ = pure $ Fix Float
  toWire = MkFloat
  fromWire (MkFloat float) = Just float
  fromWire _ = Nothing

instance Elm Text where
  wireType' _ = pure $ Fix String
  toWire = MkString
  fromWire (MkString string) = Just string
  fromWire _ = Nothing

instance Elm () where
  wireType' _ = pure . Fix $ Product []
  toWire () = MkProduct []
  fromWire _ = pure ()

instance Elm Void where
  wireType' _ = pure . Fix $ Void
  toWire = absurd
  fromWire _ = Nothing

instance Elm a => Elm [a] where
  wireType' _ = Fix . List <$> wireType' (Proxy @a)
  toWire = MkList . fmap toWire
  fromWire (MkList xs) = traverse fromWire xs
  fromWire _ = Nothing

-- Instances for tuples.
-- The 7-tuple is the largest one that has a Generics instance, so we'll support
-- up to that number here too.
instance (Elm a, Elm b) => Elm (a, b) where
  wireType' _ = tupleType [wireType' (Proxy @a), wireType' (Proxy @b)]
  toWire (a, b) = MkProduct [toWire a, toWire b]
  fromWire (MkProduct [a, b]) = (,) <$> fromWire a <*> fromWire b
  fromWire _ = Nothing

instance (Elm a, Elm b, Elm c) => Elm (a, b, c) where
  wireType' _ =
    tupleType [wireType' (Proxy @a), wireType' (Proxy @b), wireType' (Proxy @c)]
  toWire (a, b, c) = MkProduct [toWire a, toWire b, toWire c]
  fromWire (MkProduct [a, b, c]) =
    (,,) <$> fromWire a <*> fromWire b <*> fromWire c
  fromWire _ = Nothing

instance (Elm a, Elm b, Elm c, Elm d) => Elm (a, b, c, d) where
  wireType' _ =
    tupleType
      [ wireType' (Proxy @a)
      , wireType' (Proxy @b)
      , wireType' (Proxy @c)
      , wireType' (Proxy @d)
      ]
  toWire (a, b, c, d) = MkProduct [toWire a, toWire b, toWire c, toWire d]
  fromWire (MkProduct [a, b, c, d]) =
    (,,,) <$> fromWire a <*> fromWire b <*> fromWire c <*> fromWire d
  fromWire _ = Nothing

instance (Elm a, Elm b, Elm c, Elm d, Elm e) => Elm (a, b, c, d, e) where
  wireType' _ =
    tupleType
      [ wireType' (Proxy @a)
      , wireType' (Proxy @b)
      , wireType' (Proxy @c)
      , wireType' (Proxy @d)
      , wireType' (Proxy @e)
      ]
  toWire (a, b, c, d, e) =
    MkProduct [toWire a, toWire b, toWire c, toWire d, toWire e]
  fromWire (MkProduct [a, b, c, d, e]) =
    (,,,,) <$> fromWire a <*> fromWire b <*> fromWire c <*> fromWire d <*>
    fromWire e
  fromWire _ = Nothing

instance (Elm a, Elm b, Elm c, Elm d, Elm e, Elm f) =>
         Elm (a, b, c, d, e, f) where
  wireType' _ =
    tupleType
      [ wireType' (Proxy @a)
      , wireType' (Proxy @b)
      , wireType' (Proxy @c)
      , wireType' (Proxy @d)
      , wireType' (Proxy @e)
      , wireType' (Proxy @f)
      ]
  toWire (a, b, c, d, e, f) =
    MkProduct [toWire a, toWire b, toWire c, toWire d, toWire e, toWire f]
  fromWire (MkProduct [a, b, c, d, e, f]) =
    (,,,,,) <$> fromWire a <*> fromWire b <*> fromWire c <*> fromWire d <*>
    fromWire e <*>
    fromWire f
  fromWire _ = Nothing

instance (Elm a, Elm b, Elm c, Elm d, Elm e, Elm f, Elm g) =>
         Elm (a, b, c, d, e, f, g) where
  wireType' _ =
    tupleType
      [ wireType' (Proxy @a)
      , wireType' (Proxy @b)
      , wireType' (Proxy @c)
      , wireType' (Proxy @d)
      , wireType' (Proxy @e)
      , wireType' (Proxy @f)
      , wireType' (Proxy @g)
      ]
  toWire (a, b, c, d, e, f, g) =
    MkProduct
      [toWire a, toWire b, toWire c, toWire d, toWire e, toWire f, toWire g]
  fromWire (MkProduct [a, b, c, d, e, f, g]) =
    (,,,,,,) <$> fromWire a <*> fromWire b <*> fromWire c <*> fromWire d <*>
    fromWire e <*>
    fromWire f <*>
    fromWire g
  fromWire _ = Nothing

tupleType :: Applicative f => [f PrimitiveType] -> f PrimitiveType
tupleType xs = Fix . Product . fmap ("", ) <$> sequenceA xs

-- |
-- Helper class for constructing write types from generic representations of
-- types.
class ElmG (f :: * -> *) where
  wireTypeG :: Proxy f -> Builder PrimitiveType
  toWireG :: f p -> Value
  fromWireG :: Value -> Maybe (f p)

instance (Elm c) => ElmG (K1 i c) where
  wireTypeG _ = wireType' (Proxy @c)
  toWireG = toWire . unK1
  fromWireG = fmap K1 . fromWire

instance (HasName m, SumsG f) => ElmG (M1 D m f) where
  wireTypeG _ = do
    let name' = name (Proxy @m)
    namesSeen <- ask
    unless (Set.member name' namesSeen) $ do
      constructors <- local (Set.insert name') $ sumsG (Proxy @f)
      tell . UserTypes $ Map.singleton name' constructors
    pure . Fix $ User name'
  toWireG = uncurry MkSum . fmap MkProduct . toSumsG . unM1
  fromWireG (MkSum n (MkProduct xs)) = fmap M1 $ fromSumsG n xs
  fromWireG (MkSum n x) = fmap M1 $ fromSumsG n [x]
  fromWireG _ = Nothing

-- |
-- Helper class for constructing sums of types.
class SumsG (f :: * -> *) where
  sumsG :: Proxy f -> Builder [(Text, PrimitiveType)]
  toSumsG :: f p -> (NthConstructor, [Value])
  fromSumsG :: NthConstructor -> [Value] -> Maybe (f p)

instance (SumsG f, SumsG g) => SumsG (f :+: g) where
  sumsG _ = (<>) <$> sumsG (Proxy @f) <*> sumsG (Proxy @g)
  toSumsG (L1 x) = toSumsG x
  toSumsG (R1 x) = first (+ 1) $ toSumsG x
  fromSumsG 0 x = fmap L1 (fromSumsG 0 x)
  fromSumsG n x = fmap R1 (fromSumsG (n - 1) x)

instance (HasName m, ProductG f) => SumsG (M1 C m f) where
  sumsG _ = pure . (name (Proxy @m), ) . Fix . Product <$> productG (Proxy @f)
  toSumsG = (0, ) . toProductG . unM1
  fromSumsG 0 x = fmap M1 (fromProductG x)
  fromSumsG _ _ = Nothing -- ^ We picked a constructor that doesn't exist.

instance SumsG V1 where
  sumsG _ = pure []
  toSumsG = \case {}
  fromSumsG _ _ = Nothing

-- |
-- Helper class for constructing products.
class ProductG (f :: * -> *) where
  productG :: Proxy f -> Builder [(Text, PrimitiveType)]
  productLengthG :: Proxy f -> Int
  toProductG :: f p -> [Value]
  fromProductG :: [Value] -> Maybe (f p)

instance (ProductG f, ProductG g) => ProductG (f :*: g) where
  productG _ = (<>) <$> productG (Proxy @f) <*> productG (Proxy @g)
  productLengthG _ = productLengthG (Proxy @f) + productLengthG (Proxy @f)
  toProductG (x :*: y) = toProductG x <> toProductG y
  fromProductG z = (:*:) <$> fromProductG x <*> fromProductG y
    where
      (x, y) = splitAt (productLengthG (Proxy @f)) z

instance (HasName m, ElmG f) => ProductG (M1 S m f) where
  productG _ = pure . (name (Proxy @m), ) <$> wireTypeG (Proxy @f)
  productLengthG _ = 1
  toProductG = pure . toWireG . unM1
  fromProductG [x] = M1 <$> fromWireG x
  fromProductG _ = Nothing -- ^ We got the wrong number of constructors.

instance ProductG U1 where
  productG _ = pure []
  productLengthG _ = 0
  toProductG U1 = []
  fromProductG [] = Just U1
  fromProductG _ = Nothing

-- |
-- Helper class for extracting the type-level name of a `Meta` kind.
-- Used to recover names of types, constructors, and fields.
class HasName (a :: Meta) where
  name :: Proxy a -> Text

instance (KnownSymbol n, KnownSymbol m) => HasName ('MetaData n m p nt) where
  name _ = Text.pack $ symbolVal (Proxy @m) <> "." <> symbolVal (Proxy @n)

instance (KnownSymbol n) => HasName ('MetaCons n f s) where
  name _ = Text.pack $ symbolVal (Proxy @n)

instance (KnownSymbol n) => HasName ('MetaSel ('Just n) m p nt) where
  name _ = Text.pack $ symbolVal (Proxy @n)

instance HasName ('MetaSel 'Nothing m p nt) where
  name _ = ""
