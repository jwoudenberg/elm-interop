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

import Coder (Coder, WrappedField(WrappedField), (=:), handle, match)
import Data.Functor.Foldable (Fix(Fix))
import Data.Functor.Invariant (Invariant(invmap))
import Data.Vinyl (ElField, ElField(Field), Label(Label), Rec((:&), RNil))
import Data.Vinyl.CoRec (CoRec(CoRec))
import Data.Vinyl.Functor
  ( (:.)
  , Compose(Compose)
  , Identity(Identity, getIdentity)
  )
import ElmType

import qualified Coder

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
        Label @"_left" =: wrapIdentity (coder left) :& Label @"_right" =:
        wrapIdentity (coder right) :&
        RNil
    , elmType = Fix $ ElmResult {err = elmType left, ok = elmType right}
    }
  where
    wrapIdentity :: Coder a -> (Coder :. Identity) a
    wrapIdentity = Compose . invmap Identity getIdentity
    toEither coRec = match coRec $ handle Left :& handle Right :& RNil
    fromEither (Left l) =
      CoRec (WrappedField (Identity l) :: WrappedField Identity '( "_left", l))
    fromEither (Right r) =
      CoRec (WrappedField (Identity r) :: WrappedField Identity '( "_right", r))
