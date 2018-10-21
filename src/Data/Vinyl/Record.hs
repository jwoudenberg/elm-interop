{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Data.Vinyl.Record
  ( Record
  , Field(Field)
  , (=:)
  , getField
  , getLabel
  , labelVal
  , rpureConstrained
  , rmap
  , rpure
  , FieldConstrained
  , AllFieldsConstrained
  , RecordApplicative
  , singleton
  , unSingleton
  , RecAppend((+++), rappend, rsplit)
  , DropFields(WithoutFields, dropFields, addFields)
  ) where

-- |
-- An alternative version of the `Data.Vinyl.Derived` module, for representing
-- records containing key-value pairs.
--
-- The `Record` type in `Data.Vinyl.Derived` is limiting in that it does not
-- allow the value types of a `Field` to be wrapped in an interpreting functor.
-- This module is based of a different `Field` type that does allow this.
import Data.Proxy (Proxy(Proxy))
import Data.Vinyl (Rec((:&), RNil))
import GHC.Exts (Constraint)
import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)

import qualified Data.Text as T

-- |
-- Like `Field` from `Vinyl`, this signifies a key-value pair. This version
-- allows the value type to be wrapped in a functor.
data Field f (field :: (Symbol, k)) where
  Field :: KnownSymbol s => !(f a) -> Field f '( s, a)

-- |
-- A record containing key value pairs. Values are wrapped in an interpreting
-- functor.
type Record f = Rec (Field f)

-- |
-- Operator for creating a field.
(=:) :: (KnownSymbol s) => proxy (s :: Symbol) -> f a -> Field f '( s, a)
_ =: x = Field x

-- |
-- Get the value of a field.
getField :: Field f '( s, a) -> f a
getField (Field x) = x

-- |
-- Get the label of a field.
getLabel ::
     forall s f a. (KnownSymbol s)
  => Field f '( s, a)
  -> T.Text
getLabel _ = T.pack $ symbolVal (Proxy :: Proxy s)

labelVal :: (KnownSymbol s) => proxy s -> T.Text
labelVal = T.pack . symbolVal

-- |
-- Construct a 'Record' with 'Field' elements.
class RecordApplicative fs where
  rpure :: (forall a. f a) -> Record f fs

instance RecordApplicative '[] where
  rpure _ = RNil

instance (KnownSymbol s, RecordApplicative fs) =>
         RecordApplicative ('( s, f) ': fs) where
  rpure f = Field f :& rpure f

-- |
-- Map a function between functors across a 'Record' taking advantage of
-- knowledge that each element is an 'Field'.
rmap :: (forall a. f a -> g a) -> Record f fs -> Record g fs
rmap _ RNil = RNil
rmap f (Field x :& xs) = Field (f x) :& rmap f xs

-- |
-- Accessor for the type of a field.
-- Note that this is the field type unwrapped with its interpretation functor.
type family FieldType f where
  FieldType (Field g '( s, t)) = t

-- |
-- Put a constrain on the inner field type.
class (c (FieldType f)) =>
      FieldConstrained c f


instance (c t) => FieldConstrained c (Field f '( s, t))

type family AllFieldsConstrained c xs :: Constraint where
  AllFieldsConstrained c '[] = ()
  AllFieldsConstrained c ('( s, t) ': ts) = (c t, AllFieldsConstrained c ts)

rpureConstrained ::
     forall c f proxy ts. (AllFieldsConstrained c ts, RecordApplicative ts)
  => proxy c
  -> (forall a. c a =>
                  f a)
  -> Record f ts
rpureConstrained _ f = go (rpure Proxy)
  where
    go :: (AllFieldsConstrained c ts') => Record Proxy ts' -> Record f ts'
    go RNil = RNil
    go (Field Proxy :& xs) = Field f :& go xs

singleton :: (KnownSymbol s) => Proxy s -> f a -> Record f '[ '( s, a)]
singleton _ x = Field x :& RNil

unSingleton :: Record f '[ '( s, a)] -> f a
unSingleton (Field x :& RNil) = x

class RecAppend (xs :: [k]) (ys :: [k]) where
  type xs +++ ys :: [k]
  rappend :: Rec f xs -> Rec f ys -> Rec f (xs +++ ys)
  rsplit :: Rec f (xs +++ ys) -> (Rec f xs, Rec f ys)

instance RecAppend '[] ys where
  type '[] +++ ys = ys
  rappend RNil ys = ys
  rsplit ys = (RNil, ys)

instance (RecAppend xs ys) => RecAppend (x ': xs) ys where
  type (x ': xs) +++ ys = x ': (xs +++ ys)
  rappend (x :& xs) ys = x :& rappend xs ys
  rsplit (x :& zs) = (x :& xs, ys)
    where
      (xs, ys) = rsplit zs

class DropFields (xs :: [(Symbol, k)]) where
  type WithoutFields xs :: [k]
  dropFields :: Record f xs -> Rec f (WithoutFields xs)
  addFields :: Rec f (WithoutFields xs) -> Record f xs

instance DropFields '[] where
  type WithoutFields '[] = '[]
  dropFields _ = RNil
  addFields _ = RNil

instance forall xs x s. (KnownSymbol s, DropFields xs) =>
         DropFields ('( s, x) ': xs) where
  type WithoutFields ('( s, x) ': xs) = x ': WithoutFields xs
  dropFields (Field x :& xs) = x :& dropFields xs
  addFields (x :& xs) = mkField (Proxy @s) x :& addFields xs

mkField :: (KnownSymbol s) => Proxy s -> f a -> Field f '( s, a)
mkField _ = Field
