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
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Wire
  ( Type_
  , TypeName(..)
  , ConstructorName(..)
  , FieldName(..)
  , PrimitiveType
  , PrimitiveTypeF(..)
  , UserTypes(..)
  , Value(..)
  , Rep
  , wireType
  , toWire
  , fromWire
  ) where

import Control.Applicative ((<|>))
import Control.Monad (unless)
import Control.Monad.Reader (Reader, ask, local, runReader)
import Control.Monad.Writer.Strict (WriterT, runWriterT, tell)
import Data.Bifunctor (first, second)
import Data.Foldable (foldl', toList)
import Data.Functor.Foldable (Fix(Fix))
import Data.HashMap.Strict (HashMap)
import Data.HashSet (HashSet)
import Data.Hashable (Hashable)
import Data.Int (Int32)
import Data.Map.Strict (Map)
import Data.Proxy (Proxy(Proxy))
import Data.Sequence (Seq)
import Data.Sequence.Extra
  ( foldableToSeq
  , hashMapFromFoldable
  , hashMapToSeq
  , mapFromFoldable
  , mapToSeq
  )
import Data.Set (Set)
import Data.String (IsString)
import Data.Text (Text)
import Data.Tuple (swap)
import Data.Vector (Vector)
import Data.Void (Void, absurd)
import GHC.Generics hiding (Rep)
import GHC.TypeLits (KnownSymbol, symbolVal)
import Type.Reflection (Typeable)

import qualified Data.HashSet as HashSet
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Vector as Vector
import qualified GHC.Generics as Generics
import qualified Type.Reflection

type Type_ = (UserTypes, PrimitiveType)

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
  = Record (Seq (FieldName, a))
  -- ^ A record consisting of name-value pairs.
  | Tuple (Seq a)
  -- ^ A tuple or constructor parameter list.
  | User TypeName
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
  | Bool
  deriving (Functor)

type PrimitiveType = Fix PrimitiveTypeF

-- |
-- Everything that isn't a primitive type is a user type. We always keep their
-- definitions together in a map to avoid duplicates.
--
-- A user type is a Haskell sum type or Elm custom type. It can have multiple
-- constructors, each with one or more parameters.
newtype UserTypes = UserTypes
  { unUserTypes :: Map TypeName (Seq (ConstructorName, PrimitiveType))
  } deriving (Semigroup, Monoid)

-- |
-- The `PrimitiveType` describes the types of the things going over the wire
-- between Haskell and a language like Elm. This `Value` type describes the
-- values of those types we're going to encode and decode.
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
  | MkBool Bool
  | MkList (Seq Value)
  | MkRecord (Map FieldName Value)
  | MkTuple (Seq Value)
  | MkSum ConstructorName
          Value
  deriving (Generic)

data TypeName = TypeName
  { typeConstructor :: Text
  , fromModule :: Text
  , parameters :: [Text]
  } deriving (Eq, Ord)

newtype ConstructorName = ConstructorName
  { unConstructorName :: Text
  } deriving (Eq, Ord, IsString)

newtype FieldName = FieldName
  { unFieldName :: Text
  } deriving (Eq, Ord, IsString)

wireType :: Rep a => Proxy a -> Type_
wireType = swap . flip runReader mempty . runWriterT . wireType'

-- |
-- Class of types that have a wire format representation.
class Typeable a =>
      Rep (a :: *)
  where
  wireType' :: Proxy a -> Builder PrimitiveType
  toWire :: a -> Value
  fromWire :: Value -> Maybe a
  -- Default Generics-based implementations.
  default wireType' :: (WireG (Generics.Rep a)) =>
    Proxy a -> Builder PrimitiveType
  wireType' _ =
    local (first (const (paramsOf (Proxy @a)))) $
    wireTypeG (Proxy @(Generics.Rep a))
  default toWire :: (Generic a, WireG (Generics.Rep a)) =>
    a -> Value
  toWire = toWireG . from
  default fromWire :: (Generic a, WireG (Generics.Rep a)) =>
    Value -> Maybe a
  fromWire = fmap to . fromWireG

paramsOf ::
     forall a. Typeable a
  => Proxy a
  -> ParamNames
paramsOf _ =
  ParamNames . fmap (Text.pack . show) . snd . Type.Reflection.splitApps $
  Type.Reflection.typeOf (undefined :: a)

type Builder a = WriterT UserTypes (Reader (ParamNames, Set TypeName)) a

newtype ParamNames =
  ParamNames [Text]
  deriving (Semigroup, Monoid)

instance Rep Int32 where
  wireType' _ = pure $ Fix Int
  toWire = MkInt
  fromWire (MkInt int) = Just int
  fromWire _ = Nothing

instance Rep Double where
  wireType' _ = pure $ Fix Float
  toWire = MkFloat
  fromWire (MkFloat float) = Just float
  fromWire _ = Nothing

instance Rep Text where
  wireType' _ = pure $ Fix String
  toWire = MkString
  fromWire (MkString string) = Just string
  fromWire _ = Nothing

instance Rep Bool where
  wireType' _ = pure $ Fix Bool
  toWire = MkBool
  fromWire (MkBool bool) = Just bool
  fromWire _ = Nothing

instance Rep () where
  wireType' _ = pure . Fix $ Tuple mempty
  toWire () = MkTuple []
  fromWire _ = pure ()

instance Rep Void where
  wireType' _ = pure . Fix $ Void
  toWire = absurd
  fromWire _ = Nothing

instance Rep a => Rep [a] where
  wireType' _ = Fix . List <$> wireType' (Proxy @a)
  toWire = MkList . Seq.fromList . fmap toWire
  fromWire (MkList xs) = traverse fromWire $ toList xs
  fromWire _ = Nothing

instance Rep a => Rep (Seq a) where
  wireType' _ = Fix . List <$> wireType' (Proxy @a)
  toWire = MkList . fmap toWire
  fromWire (MkList xs) = traverse fromWire xs
  fromWire _ = Nothing

instance Rep a => Rep (Vector a) where
  wireType' _ = Fix . List <$> wireType' (Proxy @a)
  toWire = MkList . fmap toWire . foldableToSeq
  fromWire (MkList xs) = fmap (Vector.fromList . toList) $ traverse fromWire xs
  fromWire _ = Nothing

instance (Ord a, Rep a) => Rep (Set a) where
  wireType' _ = Fix . List <$> wireType' (Proxy @a)
  toWire = MkList . fmap toWire . foldableToSeq
  fromWire (MkList xs) =
    fmap (foldl' (flip Set.insert) mempty) $ traverse fromWire xs
  fromWire _ = Nothing

instance (Eq a, Hashable a, Rep a) => Rep (HashSet a) where
  wireType' _ = Fix . List <$> wireType' (Proxy @a)
  toWire = MkList . fmap toWire . foldableToSeq
  fromWire (MkList xs) =
    fmap (foldl' (flip HashSet.insert) mempty) $ traverse fromWire xs
  fromWire _ = Nothing

instance (Ord k, Rep k, Rep v) => Rep (Map k v) where
  wireType' _ =
    Fix . List <$> tupleType [wireType' (Proxy @k), wireType' (Proxy @v)]
  toWire = MkList . fmap (\(k, v) -> MkTuple [toWire k, toWire v]) . mapToSeq
  fromWire (MkList xs) = mapFromFoldable <$> traverse toTuple xs
    where
      toTuple :: Value -> Maybe (k, v)
      toTuple (MkTuple [k, v]) = (,) <$> fromWire k <*> fromWire v
      toTuple _ = Nothing
  fromWire _ = Nothing

instance (Eq k, Hashable k, Rep k, Rep v) => Rep (HashMap k v) where
  wireType' _ =
    Fix . List <$> tupleType [wireType' (Proxy @k), wireType' (Proxy @v)]
  toWire =
    MkList . fmap (\(k, v) -> MkTuple [toWire k, toWire v]) . hashMapToSeq
  fromWire (MkList xs) = hashMapFromFoldable <$> traverse toTuple xs
    where
      toTuple :: Value -> Maybe (k, v)
      toTuple (MkTuple [k, v]) = (,) <$> fromWire k <*> fromWire v
      toTuple _ = Nothing
  fromWire _ = Nothing

-- Instances for tuples.
-- The 7-tuple is the largest one that has a Generics instance, so we'll support
-- up to that number here too.
instance (Rep a, Rep b) => Rep (a, b) where
  wireType' _ = tupleType [wireType' (Proxy @a), wireType' (Proxy @b)]
  toWire (a, b) = MkTuple [toWire a, toWire b]
  fromWire (MkTuple [a, b]) = (,) <$> fromWire a <*> fromWire b
  fromWire _ = Nothing

instance (Rep a, Rep b, Rep c) => Rep (a, b, c) where
  wireType' _ =
    tupleType [wireType' (Proxy @a), wireType' (Proxy @b), wireType' (Proxy @c)]
  toWire (a, b, c) = MkTuple [toWire a, toWire b, toWire c]
  fromWire (MkTuple [a, b, c]) =
    (,,) <$> fromWire a <*> fromWire b <*> fromWire c
  fromWire _ = Nothing

instance (Rep a, Rep b, Rep c, Rep d) => Rep (a, b, c, d) where
  wireType' _ =
    tupleType
      [ wireType' (Proxy @a)
      , wireType' (Proxy @b)
      , wireType' (Proxy @c)
      , wireType' (Proxy @d)
      ]
  toWire (a, b, c, d) = MkTuple [toWire a, toWire b, toWire c, toWire d]
  fromWire (MkTuple [a, b, c, d]) =
    (,,,) <$> fromWire a <*> fromWire b <*> fromWire c <*> fromWire d
  fromWire _ = Nothing

instance (Rep a, Rep b, Rep c, Rep d, Rep e) => Rep (a, b, c, d, e) where
  wireType' _ =
    tupleType
      [ wireType' (Proxy @a)
      , wireType' (Proxy @b)
      , wireType' (Proxy @c)
      , wireType' (Proxy @d)
      , wireType' (Proxy @e)
      ]
  toWire (a, b, c, d, e) =
    MkTuple [toWire a, toWire b, toWire c, toWire d, toWire e]
  fromWire (MkTuple [a, b, c, d, e]) =
    (,,,,) <$> fromWire a <*> fromWire b <*> fromWire c <*> fromWire d <*>
    fromWire e
  fromWire _ = Nothing

instance (Rep a, Rep b, Rep c, Rep d, Rep e, Rep f) =>
         Rep (a, b, c, d, e, f) where
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
    MkTuple [toWire a, toWire b, toWire c, toWire d, toWire e, toWire f]
  fromWire (MkTuple [a, b, c, d, e, f]) =
    (,,,,,) <$> fromWire a <*> fromWire b <*> fromWire c <*> fromWire d <*>
    fromWire e <*>
    fromWire f
  fromWire _ = Nothing

instance (Rep a, Rep b, Rep c, Rep d, Rep e, Rep f, Rep g) =>
         Rep (a, b, c, d, e, f, g) where
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
    MkTuple
      [toWire a, toWire b, toWire c, toWire d, toWire e, toWire f, toWire g]
  fromWire (MkTuple [a, b, c, d, e, f, g]) =
    (,,,,,,) <$> fromWire a <*> fromWire b <*> fromWire c <*> fromWire d <*>
    fromWire e <*>
    fromWire f <*>
    fromWire g
  fromWire _ = Nothing

tupleType :: Applicative f => Seq (f PrimitiveType) -> f PrimitiveType
tupleType xs = Fix . Tuple <$> sequenceA xs

-- |
-- Helper class for constructing write types from generic representations of
-- types.
class WireG (f :: * -> *) where
  wireTypeG :: Proxy f -> Builder PrimitiveType
  toWireG :: f p -> Value
  fromWireG :: Value -> Maybe (f p)

instance (Rep c) => WireG (K1 i c) where
  wireTypeG _ = wireType' (Proxy @c)
  toWireG = toWire . unK1
  fromWireG = fmap K1 . fromWire

instance (HasTypeName m, SumsG f) => WireG (M1 D m f) where
  wireTypeG _ = do
    (unqualifiedName, namesSeen) <- ask
    let name = typeName (Proxy @m) unqualifiedName
    unless (Set.member name namesSeen) $ do
      constructors <- local (second (Set.insert name)) $ sumsG (Proxy @f)
      tell . UserTypes $ Map.singleton name constructors
    pure . Fix $ User name
  toWireG = uncurry MkSum . toSumsG . unM1
  fromWireG (MkSum n x) = fmap M1 $ fromSumsG n x
  fromWireG _ = Nothing

-- |
-- Helper class for constructing sums of types.
class SumsG (f :: * -> *) where
  sumsG :: Proxy f -> Builder (Seq (ConstructorName, PrimitiveType))
  toSumsG :: f p -> (ConstructorName, Value)
  fromSumsG :: ConstructorName -> Value -> Maybe (f p)

instance (SumsG f, SumsG g) => SumsG (f :+: g) where
  sumsG _ = (<>) <$> sumsG (Proxy @f) <*> sumsG (Proxy @g)
  toSumsG (L1 x) = toSumsG x
  toSumsG (R1 x) = toSumsG x
  fromSumsG n x = fmap L1 (fromSumsG n x) <|> fmap R1 (fromSumsG n x)

instance (KnownSymbol n, ProductG f) =>
         SumsG (M1 C ('MetaCons n fi 'True) f) where
  sumsG _ = pure . (name, ) . Fix . Record <$> productG (Proxy @f)
    where
      name = constructorName (Proxy @n)
  toSumsG =
    (constructorName (Proxy @n), ) .
    MkRecord . mapFromFoldable . toProductG . unM1
  fromSumsG n (MkRecord x)
    | n == constructorName (Proxy @n) = fmap M1 (fromProductG (mapToSeq x))
  fromSumsG _ _ = Nothing -- ^ We picked a constructor that doesn't exist.

instance (KnownSymbol n, ProductG f) =>
         SumsG (M1 C ('MetaCons n fi 'False) f) where
  sumsG _ = pure . (name, ) . Fix . Tuple . fmap snd <$> productG (Proxy @f)
    where
      name = constructorName (Proxy @n)
  toSumsG =
    (constructorName (Proxy @n), ) . MkTuple . fmap snd . toProductG . unM1
  fromSumsG n (MkTuple xs)
    | n == constructorName (Proxy @n) = fmap M1 (fromProductG $ ("", ) <$> xs)
  fromSumsG _ _ = Nothing -- ^ We picked a constructor that doesn't exist.

instance SumsG V1 where
  sumsG _ = pure []
  toSumsG = \case {}
  fromSumsG _ _ = Nothing

-- |
-- Helper class for constructing products.
class ProductG (f :: * -> *) where
  productG :: Proxy f -> Builder (Seq (FieldName, PrimitiveType))
  productLengthG :: Proxy f -> Int
  toProductG :: f p -> (Seq (FieldName, Value))
  fromProductG :: (Seq (FieldName, Value)) -> Maybe (f p)

instance (ProductG f, ProductG g) => ProductG (f :*: g) where
  productG _ = (<>) <$> productG (Proxy @f) <*> productG (Proxy @g)
  productLengthG _ = productLengthG (Proxy @f) + productLengthG (Proxy @f)
  toProductG (x :*: y) = toProductG x <> toProductG y
  fromProductG z = (:*:) <$> fromProductG x <*> fromProductG y
    where
      (x, y) = Seq.splitAt (productLengthG (Proxy @f)) z

instance (HasFieldName m, WireG f) => ProductG (M1 S m f) where
  productG _ = pure . (name, ) <$> wireTypeG (Proxy @f)
    where
      name = fieldName (Proxy @m)
  productLengthG _ = 1
  toProductG = pure . (fieldName (Proxy @m), ) . toWireG . unM1
  fromProductG [(_, x)] = M1 <$> fromWireG x
  fromProductG _ = Nothing -- ^ We got the wrong number of constructors.

instance ProductG U1 where
  productG _ = pure []
  productLengthG _ = 0
  toProductG U1 = []
  fromProductG [] = Just U1
  fromProductG _ = Nothing

-- |
-- Helper classes for extracting the type-level names of a `Meta` kind.
-- Used to recover names of types, constructors, and fields.
class HasTypeName (a :: Meta) where
  typeName :: Proxy a -> ParamNames -> TypeName

instance (KnownSymbol m, KnownSymbol n) =>
         HasTypeName ('MetaData n m p nt) where
  typeName _ (ParamNames params) =
    TypeName
      { typeConstructor = Text.pack . symbolVal $ Proxy @n
      , fromModule = Text.pack . symbolVal $ Proxy @m
      , parameters = params
      }

constructorName ::
     forall n. (KnownSymbol n)
  => Proxy n
  -> ConstructorName
constructorName _ = ConstructorName . Text.pack $ symbolVal (Proxy @n)

class HasFieldName (a :: Meta) where
  fieldName :: Proxy a -> FieldName

instance (KnownSymbol n) => HasFieldName ('MetaSel ('Just n) m p nt) where
  fieldName _ = FieldName . Text.pack $ symbolVal (Proxy @n)

instance HasFieldName ('MetaSel 'Nothing m p nt) where
  fieldName _ = ""
