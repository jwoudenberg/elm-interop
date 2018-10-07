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

import Coder (Coder, handle, match)
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
  , reifyConstraint
  , rfoldMap
  , rmap
  , rmapf
  , rpureConstrained
  , rpuref
  )
import Data.Vinyl.CoRec (CoRec, FoldRec)
import Data.Vinyl.Functor
  ( (:.)
  , Compose(Compose)
  , Identity(Identity, getIdentity)
  )
import Data.Vinyl.Record
  ( Field(Field)
  , Record
  , RecordApplicative
  , (=:)
  , getLabel
  )
import Data.Vinyl.TypeLevel (AllConstrained, Fst, RecAll, Snd)
import ElmType
import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)
import ToElm (ToElm(ToElm, coder, elmType))

import qualified Coder
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text as T
import qualified Data.Vinyl as Vinyl
import qualified Data.Vinyl.Record as Record
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

class HasElmField a where
  type FieldElmType a
  hasElmField :: a -> ToElm (FieldElmType a)

instance (HasElm a, KnownSymbol s) => HasElmField (Field Proxy '( s, a)) where
  type FieldElmType (Field Proxy '( s, a)) = a
  hasElmField (Field x) = mkToElm x

instance forall xs. ( RecAll (Field Proxy) xs HasElmField
                    , RecApplicative xs
                    , AllFields xs
                    , RecordApplicative xs
         ) =>
         HasElm (Record Identity xs) where
  hasElm =
    ToElm
      { coder = Coder.record $ codersRec (Proxy @xs)
      , elmType = Fix . ElmRecord . rfoldMap toElmField $ toElms (Proxy @xs)
      }

codersRec ::
     (RecAll (Field Proxy) xs HasElmField, AllFields xs, RecordApplicative xs)
  => Proxy xs
  -> Record Coder xs
codersRec p = Record.rmap coder (toElms p)

toElms ::
     (RecAll (Field Proxy) xs HasElmField, AllFields xs, RecordApplicative xs)
  => Proxy xs
  -> Record ToElm xs
toElms _ = rmapf getToElm hasElmFieldRec
  where
    getToElm ::
         (KnownSymbol s)
      => (Dict HasElmField :. Field Proxy) '( s, t)
      -> Field ToElm '( s, t)
    getToElm (Compose (Dict x)) = Field (hasElmField x)

hasElmFieldRec ::
     (RecAll (Field Proxy) xs HasElmField, RecordApplicative xs)
  => Rec (Dict HasElmField :. Field Proxy) xs
hasElmFieldRec =
  reifyConstraint (Proxy :: Proxy HasElmField) (Record.rpure Proxy)

toElmField :: Field ToElm a -> HashMap T.Text ElmType
toElmField t@(Field toElm) = HashMap.singleton (getLabel t) (elmType toElm)

mkToElm :: (HasElm a) => proxy a -> ToElm a
mkToElm _ = hasElm

type SOP f xs = CoRec (Field (Rec f)) xs

instance forall s x xs. ( KnownSymbol s
                        , RecAll (Field Proxy) (x ': xs) HasElmField
                        , RecApplicative (x ': xs)
                        , RecordApplicative (x ': xs)
                        , AllFields (x ': xs)
                        , FoldRec (x ': xs) (x ': xs)
         ) =>
         HasElm (Label s, CoRec (Field Identity) (x ': xs)) where
  hasElm =
    ToElm
      { coder =
          invmap (Label, ) snd . Coder.union . Record.rmap wrapIdentity $
          codersRec (Proxy @(x ': xs))
      , elmType =
          Fix .
          ElmCustomType (T.pack . symbolVal $ Proxy @s) .
          rfoldMap toElmConstructor $
          toElms (Proxy @(x ': xs))
      }
    where
      wrapIdentity = Compose . invmap Identity getIdentity

toElmConstructor :: Field ToElm a -> HashMap T.Text [ElmType]
toElmConstructor t@(Field toElm) =
  HashMap.singleton (getLabel t) [elmType toElm]
