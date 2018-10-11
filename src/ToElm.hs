{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module ToElm
  ( ToElm(ToElm, coder, elmType)
  , int
  , string
  , list
  , tuple2
  , ToElm.either
  ) where

import Coder (Coder)
import Data.Functor.Foldable (Fix(Fix))
import Data.Functor.Invariant (Invariant(invmap))
import Data.Vinyl (Label(Label), Rec((:&), RNil))
import Data.Vinyl.CoRec (CoRec(CoRec))
import Data.Vinyl.Functor
  ( (:.)
  , Compose(Compose)
  , Identity(Identity, getIdentity)
  )
import Data.Vinyl.Record (Field(Field), (=:), getField)
import ElmType

import qualified Coder
import qualified Data.Vinyl
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

either :: forall l r. ToElm l -> ToElm r -> ToElm (Either l r)
either left right =
  ToElm
    { coder =
        invmap toEither fromEither . Coder.union $
        Label @"_left" =: coder left :& Label @"_right" =: coder right :& RNil
    , elmType = Fix $ ElmResult {err = elmType left, ok = elmType right}
    }
  where
    toEither coRec = Sum.match coRec $ handle Left :& handle Right :& RNil
    fromEither (Left l) =
      CoRec (Field (Identity l) :: Field Identity '( "_left", l))
    fromEither (Right r) =
      CoRec (Field (Identity r) :: Field Identity '( "_right", r))
    handle f = Compose (Sum.Op (f . getIdentity . getField))
