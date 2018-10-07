{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Vinyl.Record where

-- |
-- An alternative version of the `Data.Vinyl.Derived` module, for representing
-- records containing key-value pairs.
--
-- The `Record` type in `Data.Vinyl.Derived` is limiting in that it does not
-- allow the value types of a `Field` to be wrapped in an interpreting functor.
-- This module is based of a different `Field` type that does allow this.
import Data.Proxy (Proxy(Proxy))
import Data.Vinyl (AllFields, KnownField, Rec, rapply, rpureConstrained)
import Data.Vinyl.Functor (Lift(Lift))
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

-- |
-- Construct a 'Record' with 'Field' elements.
rpure ::
     AllFields fs
  => (forall a. KnownField a =>
                  Field f a)
  -> Record f fs
rpure = rpureConstrained (Proxy :: Proxy KnownField)

-- |
-- Map a function between functors across a 'Record' taking advantage of
-- knowledge that each element is an 'Field'.
rmap ::
     AllFields fs
  => (forall a. KnownField a =>
                  Field f a -> Field g a)
  -> Record f fs
  -> Record g fs
rmap f = rapply $ rpureConstrained (Proxy :: Proxy KnownField) (Lift f)
