{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}

module ToElm where

import Coder (Coder, (=:), handle, match)
import Data.Aeson.Types (Parser)
import Data.Bifunctor (bimap)
import Data.Functor.Invariant (invmap)
import Data.Vinyl (ElField(Field), Label(Label), Rec((:&), RNil))
import Data.Vinyl.CoRec (CoRec(CoRec))
import GHC.Exts (fromList, toList)

import qualified Coder
import qualified Data.Aeson as Aeson
import qualified Data.Text as T

data ToElm a = ToElm
  { coder :: Coder a
  , elmType :: ElmType
  }

class HasElm a where
  hasElm :: ToElm a

data ElmType
  = ElmInt
  | ElmString
  | ElmList ElmType
  | ElmTuple2 ElmType
              ElmType
  | ElmResult { ok :: ElmType
              , err :: ElmType }

int :: ToElm Int
int = ToElm {coder = Coder.primitive, elmType = ElmInt}

instance HasElm Int where
  hasElm = int

string :: ToElm String
string = ToElm {coder = Coder.primitive, elmType = ElmString}

instance HasElm String where
  hasElm = string

list :: ToElm a -> ToElm [a]
list x = ToElm {coder = Coder.many (coder x), elmType = ElmList (elmType x)}

instance HasElm a => HasElm [a] where
  hasElm = list hasElm

tuple2 :: ToElm a -> ToElm b -> ToElm (a, b)
tuple2 first second =
  ToElm
    { coder = Coder.tuple2 (coder first) (coder second)
    , elmType = ElmTuple2 (elmType first) (elmType second)
    }

instance (HasElm a, HasElm b) => HasElm (a, b) where
  hasElm = tuple2 hasElm hasElm

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

instance (HasElm l, HasElm r) => HasElm (Either l r) where
  hasElm = ToElm.either hasElm hasElm
