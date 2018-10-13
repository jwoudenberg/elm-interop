{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}

module ToElm
  ( ToElm(coder, elmType)
  , int
  , string
  , list
  , tuple2
  , ToElm.either
  , record
  , custom
  ) where

import Coder (Coder)
import Data.Functor.Foldable (Fix(Fix))
import Data.Functor.Invariant (Invariant(invmap))
import Data.HashMap.Strict (HashMap)
import Data.Proxy (Proxy(Proxy))
import Data.Vinyl (Label(Label), Rec((:&), RNil), rfoldMap, rmap)
import Data.Vinyl.CoRec (CoRec(CoRec), FoldRec)
import Data.Vinyl.Functor
  ( (:.)
  , Compose(Compose)
  , Identity(Identity, getIdentity)
  )
import Data.Vinyl.POP (POP)
import Data.Vinyl.Record (Field(Field), Record, (=:), getField, getLabel)
import Data.Vinyl.SOP (SOP)
import ElmType
import GHC.TypeLits (KnownSymbol, symbolVal)

import qualified Coder
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text as T
import qualified Data.Vinyl
import qualified Data.Vinyl.Record as Record
import qualified Data.Vinyl.Sum as Sum

data ToElm a = ToElm
  { coder :: Coder a
  , elmType :: ElmType
  }

instance Invariant ToElm where
  invmap to from (ToElm coder elmType) = ToElm (invmap to from coder) elmType

int :: ToElm Int
int = ToElm {coder = Coder.primitive, elmType = Fix ElmInt}

string :: ToElm String
string = ToElm {coder = Coder.primitive, elmType = Fix ElmString}

list :: ToElm a -> ToElm [a]
list x =
  ToElm {coder = Coder.many (coder x), elmType = Fix $ ElmList (elmType x)}

tuple2 :: ToElm a -> ToElm b -> ToElm (a, b)
tuple2 first second =
  ToElm
    { coder = Coder.tuple2 (coder first) (coder second)
    , elmType = Fix $ ElmTuple2 (elmType first) (elmType second)
    }

type EitherSOP f l r = SOP f '[ '( "_left", '[ l]), '( "_right", '[ r])]

either :: forall l r. ToElm l -> ToElm r -> ToElm (Either l r)
either left right =
  ToElm
    { coder =
        invmap toEither fromEither . Coder.union $
        Label @"_left" =: (coder left :& RNil) :& Label @"_right" =:
        (coder right :& RNil) :&
        RNil
    , elmType = Fix $ ElmResult {err = elmType left, ok = elmType right}
    }
  where
    toEither :: EitherSOP Identity l r -> Either l r
    toEither coRec = Sum.match coRec $ handle Left :& handle Right :& RNil
    fromEither :: Either l r -> EitherSOP Identity l r
    fromEither (Left l) =
      CoRec
        (Field (Identity l :& RNil) :: Field (Rec Identity) '( "_left", '[ l]))
    fromEither (Right r) =
      CoRec
        (Field (Identity r :& RNil) :: Field (Rec Identity) '( "_right", '[ r]))
    handle f = Compose (Sum.Op (f . getIdentity . extract . getField))
    extract :: Rec f '[ x] -> f x
    extract (x :& RNil) = x

record :: Record ToElm xs -> ToElm (Record Identity xs)
record rec =
  ToElm
    { coder = Coder.record (rmap (\(Field toElm) -> Field (coder toElm)) rec)
    , elmType = Fix . ElmRecord . rfoldMap fieldElmType $ rec
    }
  where
    fieldElmType :: Field ToElm a -> HashMap T.Text ElmType
    fieldElmType t@(Field toElm) =
      HashMap.singleton (getLabel t) (elmType toElm)

custom ::
     forall xs xss s. (KnownSymbol s, FoldRec (xs ': xss) (xs ': xss))
  => (Label s, POP ToElm (xs ': xss))
  -> ToElm (Label s, SOP Identity (xs ': xss))
custom (l, constructors) =
  ToElm
    { coder =
        invmap (l, ) snd . Coder.union . Record.rmap (rmap coder) $ constructors
    , elmType =
        Fix .
        ElmCustomType (T.pack . symbolVal $ Proxy @s) .
        rfoldMap toElmConstructor $
        constructors
    }
  where
    toElmConstructor :: Field (Rec ToElm) a -> HashMap T.Text [ElmType]
    toElmConstructor t@(Field toElm) =
      HashMap.singleton (getLabel t) (rfoldMap (pure . elmType) toElm)
