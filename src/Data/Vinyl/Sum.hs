{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeOperators #-}

module Data.Vinyl.Sum
  ( Sum
  , match
  -- Re-exports so this module can be used instead of `Data.Vinyl.CoRec`.
  , Handler(H)
  , matchNil
  ) where

import Data.Proxy (Proxy(Proxy))
import Data.Vinyl (Rec, rget)
import Data.Vinyl.CoRec (CoRec(CoRec), Handler(H), matchNil)
import Data.Vinyl.Functor ((:.), Compose(Compose))
import Data.Vinyl.Record (Field(Field))

-- |
-- The counterpart of Vinyls `FieldRec`, but for `CoRec`s. Contains the same
-- custom field type by our custom `Record` type with the same trade-offs.
type Sum f = CoRec (Field f)

-- |
-- Custom version of `match` for our custom `Record` and `Sum` types.
match :: Sum f xs -> Rec (Handler b :. Field f) xs -> b
match (CoRec x) hs =
  case rget Proxy hs of
    Compose (H f) -> f x
