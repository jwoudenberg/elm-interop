{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Data.Vinyl.Sum
  ( Sum
  , match
  , Op(Op)
  , singleton
  , unSingleton
  -- Re-exports so this module can be used instead of `Data.Vinyl.CoRec`.
  , matchNil
  ) where

import Data.Proxy (Proxy(Proxy))
import Data.Vinyl (Rec((:&), RNil), rget)
import Data.Vinyl.CoRec (CoRec(CoRec), matchNil)
import Data.Vinyl.Functor ((:.), Compose(Compose))
import Data.Vinyl.Record (Field(Field), getField)
import GHC.TypeLits (KnownSymbol)

-- |
-- The counterpart of Vinyls `FieldRec`, but for `CoRec`s. Contains the same
-- custom field type by our custom `Record` type with the same trade-offs.
type Sum f = CoRec (Field f)

-- |
-- Same as `Op` type from the contravariant package.
-- Bundled here so we don't need to take a dependency on the entire library
-- for a single type.
newtype Op a b =
  Op (b -> a)

-- |
-- Custom version of `match` for our custom `Record` and `Sum` types.
match :: Sum f xs -> Rec (Op b :. Field f) xs -> b
match (CoRec x) hs =
  case rget Proxy hs of
    Compose (Op f) -> f x

singleton ::
     forall s f x. (KnownSymbol s)
  => Proxy s
  -> f x
  -> Sum f '[ '( s, x)]
singleton _ x = CoRec field
  where
    field :: Field f '( s, x)
    field = Field x

unSingleton :: Sum f '[ '( s, x)] -> f x
unSingleton = flip match (Compose (Op getField) :& RNil)
