{-# LANGUAGE GADTs #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

module HasElm
  ( HasElm(hasElm)
  ) where

import Coder (Coder)
import Data.Functor.Foldable (Fix(Fix))
import Data.Functor.Invariant (Invariant(invmap))
import Data.HashMap.Strict (HashMap)
import Data.Proxy (Proxy(Proxy))
import Data.Vinyl
  ( AllFields
  , Dict(Dict)
  , KnownField
  , Label(Label)
  , Rec((:&), RNil)
  , RecApplicative
  , rfoldMap
  , rmap
  , rmapf
  , rpuref
  )
import Data.Vinyl.CoRec (CoRec, FoldRec)
import Data.Vinyl.Functor
  ( (:.)
  , Compose(Compose)
  , Identity(Identity, getIdentity)
  )
import Data.Vinyl.Record
  ( AllFieldsConstrained
  , Field(Field)
  , FieldConstrained
  , Record
  , RecordApplicative
  , (=:)
  , getLabel
  , rpureConstrained
  )
import Data.Vinyl.Sum (Sum)
import Data.Vinyl.TypeLevel (AllConstrained, Fst, RecAll, Snd)
import ElmType
import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)
import ToElm (ToElm(ToElm, coder, elmType))

import qualified Coder
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text as T
import qualified Data.Vinyl as Vinyl
import qualified Data.Vinyl.Record as Record
import qualified Data.Vinyl.Sum as Sum
import qualified ToElm

class HasElm a where
  hasElm :: ToElm a

instance HasElm Int where
  hasElm = ToElm.int

instance HasElm a => HasElm [a] where
  hasElm = ToElm.list hasElm

instance (HasElm a, HasElm b) => HasElm (a, b) where
  hasElm = ToElm.tuple2 hasElm hasElm

instance (HasElm l, HasElm r) => HasElm (Either l r) where
  hasElm = ToElm.either hasElm hasElm

instance forall xs. ( AllFieldsConstrained HasElm xs
                    , RecApplicative xs
                    , AllFields xs
                    , RecordApplicative xs
         ) =>
         HasElm (Record Identity xs) where
  hasElm =
    ToElm
      { coder = Coder.record $ codersRec (Proxy @xs)
      , elmType = Fix . ElmRecord . rfoldMap fieldElmType $ toElmRec (Proxy @xs)
      }

codersRec ::
     (AllFieldsConstrained HasElm xs, AllFields xs, RecordApplicative xs)
  => Proxy xs
  -> Record Coder xs
codersRec p = Record.rmap coder (toElmRec p)

toElmRec ::
     (AllFieldsConstrained HasElm xs, RecordApplicative xs)
  => Proxy xs
  -> Record ToElm xs
toElmRec _ = rpureConstrained (Proxy @HasElm) hasElm

fieldElmType :: Field ToElm a -> HashMap T.Text ElmType
fieldElmType t@(Field toElm) = HashMap.singleton (getLabel t) (elmType toElm)

instance forall s x xs. ( KnownSymbol s
                        , AllFieldsConstrained HasElm (x ': xs)
                        , RecApplicative (x ': xs)
                        , RecordApplicative (x ': xs)
                        , AllFields (x ': xs)
                        , FoldRec (x ': xs) (x ': xs)
         ) =>
         HasElm (Label s, Sum Identity (x ': xs)) where
  hasElm =
    ToElm
      { coder =
          invmap (Label, ) snd . Coder.union $ codersRec (Proxy @(x ': xs))
      , elmType =
          Fix .
          ElmCustomType (T.pack . symbolVal $ Proxy @s) .
          rfoldMap toElmConstructor $
          toElmRec (Proxy @(x ': xs))
      }

toElmConstructor :: Field ToElm a -> HashMap T.Text [ElmType]
toElmConstructor t@(Field toElm) =
  HashMap.singleton (getLabel t) [elmType toElm]
