{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Vinyl.Sum
  ( Sum
  , match
  , Op(Op)
  , singleton
  , unSingleton
  , CoRecAppend(cappend, csplit)
  -- Re-exports so this module can be used instead of `Data.Vinyl.CoRec`.
  , matchNil
  ) where

import Data.Maybe (fromJust)
import Data.Proxy (Proxy(Proxy))
import Data.Vinyl (Rec((:&), RNil), RecApplicative, rget)
import Data.Vinyl.CoRec
  ( CoRec(CoRec)
  , FoldRec
  , coRecToRec
  , firstField
  , matchNil
  )
import Data.Vinyl.Functor ((:.), Compose(Compose))
import Data.Vinyl.Record (Field(Field), getField)
import Data.Vinyl.TypeLevel (type (++))
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

class CoRecAppend (xs :: [k]) (ys :: [k]) where
  cappend :: Either (CoRec f xs) (CoRec f ys) -> CoRec f (xs ++ ys)
  csplit :: CoRec f (xs ++ ys) -> Either (CoRec f xs) (CoRec f ys)

instance CoRecAppend '[] ys where
  cappend (Left x) = matchNil x
  cappend (Right x) = x
  csplit = Right

instance forall x xs ys. ( CoRecAppend xs ys
                         , RecApplicative (xs ++ ys)
                         , RecApplicative xs
                         , FoldRec xs xs
                         , FoldRec (xs ++ ys) (xs ++ ys)
                         , FoldRec (x ': (xs ++ ys)) (xs ++ ys)
                         , FoldRec (x ': xs) xs
         ) =>
         CoRecAppend (x ': xs) ys where
  cappend (Left coRec) =
    case restrictCoRec coRec of
      Left x -> CoRec x
      Right xs -> weakenCoRec $ cappend' Proxy (Proxy @ys) (Left xs)
  cappend (Right ys) = weakenCoRec $ cappend' (Proxy @xs) Proxy (Right ys)
  csplit coRec =
    case restrictCoRec coRec of
      Left x -> Left (CoRec x)
      Right xs -> mapLeft weakenCoRec $ csplit' (Proxy @xs) (Proxy @ys) xs

mapLeft :: (a -> b) -> Either a c -> Either b c
mapLeft f (Left l) = Left (f l)
mapLeft _ (Right r) = Right r

cappend' ::
     (CoRecAppend xs ys)
  => proxy xs
  -> proxy ys
  -> Either (CoRec f xs) (CoRec f ys)
  -> CoRec f (xs ++ ys)
cappend' _ _ = cappend

csplit' ::
     (CoRecAppend xs ys)
  => proxy xs
  -> proxy ys
  -> CoRec f (xs ++ ys)
  -> Either (CoRec f xs) (CoRec f ys)
csplit' _ _ = csplit

-- type r ∈ rs = RElem r rs (RIndex r rs)
-- | A 'CoRec' is either the first possible variant indicated by its
-- type, or a 'CoRec' that must be one of the remaining types.
-- Taken from Vinyl version 10.0.
restrictCoRec ::
     forall t ts f. (RecApplicative ts, FoldRec ts ts)
  => CoRec f (t ': ts)
  -> Either (f t) (CoRec f ts)
restrictCoRec = go . coRecToRec
  where
    go :: Rec (Maybe :. f) (t ': ts) -> Either (f t) (CoRec f ts)
    go (Compose Nothing :& xs) = Right (fromJust (firstField xs))
    go (Compose (Just x) :& _) = Left x

-- |
-- A 'CoRec' whose possible types are @ts@ may be used at a type of
-- 'CoRec' whose possible types are @t:ts@.
-- Taken from Vinyl version 10.0.
weakenCoRec ::
     (RecApplicative ts, FoldRec (t ': ts) (t ': ts))
  => CoRec f ts
  -> CoRec f (t ': ts)
weakenCoRec = fromJust . firstField . (Compose Nothing :&) . coRecToRec
