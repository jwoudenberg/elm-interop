{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Data.Vinyl.Record
  ( Record
  , Field(Field)
  , (=:)
  , getField
  , getLabel
  , rpureConstrained
  , rmap
  , FieldConstrained
  , AllFieldsConstrained
  , RecordApplicative
  ) where

-- |
-- An alternative version of the `Data.Vinyl.Derived` module, for representing
-- records containing key-value pairs.
--
-- The `Record` type in `Data.Vinyl.Derived` is limiting in that it does not
-- allow the value types of a `Field` to be wrapped in an interpreting functor.
-- This module is based of a different `Field` type that does allow this.
import Data.Proxy (Proxy(Proxy))
import Data.Vinyl (Dict, KnownField, Rec((:&), RNil), rapply)
import Data.Vinyl.Functor ((:.), Lift(Lift))
import GHC.Exts (Constraint)
import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)

import qualified Data.Text as T
import qualified Data.Vinyl as Vinyl

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

type family AllFieldsConstrained xs c :: Constraint where
  AllFieldsConstrained '[] c = ()
  AllFieldsConstrained ('( s, t) ': ts) c = (c t, AllFieldsConstrained ts c)

rpureConstrained ::
     forall c f proxy ts. (AllFieldsConstrained ts c, RecordApplicative ts)
  => proxy c
  -> (forall a. c a =>
                  f a)
  -> Record f ts
rpureConstrained _ f = go (rpure Proxy)
  where
    go :: (AllFieldsConstrained ts' c) => Record Proxy ts' -> Record f ts'
    go RNil = RNil
    go (Field Proxy :& xs) = Field f :& go xs
