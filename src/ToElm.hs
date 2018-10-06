{-# LANGUAGE GADTs #-}
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
import Data.Functor.Invariant (Invariant(invmap))
import Data.Vinyl (ElField, ElField(Field), Label(Label), Rec((:&), RNil))
import Data.Vinyl.CoRec (CoRec(CoRec))
import ElmType

import qualified Coder

data ToElm a = ToElm
  { coder :: Coder a
  , elmType :: ElmType
  }

instance Invariant ToElm where
  invmap to from (ToElm coder elmType) = ToElm (invmap to from coder) elmType

int :: ToElm Int
int = ToElm {coder = Coder.primitive, elmType = ElmInt}

string :: ToElm String
string = ToElm {coder = Coder.primitive, elmType = ElmString}

list :: ToElm a -> ToElm [a]
list x = ToElm {coder = Coder.many (coder x), elmType = ElmList (elmType x)}

tuple2 :: ToElm a -> ToElm b -> ToElm (a, b)
tuple2 first second =
  ToElm
    { coder = Coder.tuple2 (coder first) (coder second)
    , elmType = ElmTuple2 (elmType first) (elmType second)
    }

either :: forall l r. ToElm l -> ToElm r -> ToElm (Either l r)
either left right =
  ToElm
    { coder =
        invmap toEither fromEither . Coder.union $
        Label @"_left" =: coder left :& Label @"_right" =: coder right :& RNil
    , elmType = ElmResult {err = elmType left, ok = elmType right}
    }
  where
    toEither coRec = match coRec $ handle Left :& handle Right :& RNil
    fromEither (Left l) = CoRec (Field l :: ElField '( "_left", l))
    fromEither (Right r) = CoRec (Field r :: ElField '( "_right", r))
