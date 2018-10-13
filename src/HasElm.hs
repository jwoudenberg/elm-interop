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
import Data.Vinyl.POP (AllFieldsAllConstrained, POP)
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
import Data.Vinyl.SOP (SOP)
import Data.Vinyl.Sum (Sum)
import Data.Vinyl.TypeLevel (AllConstrained, Fst, RecAll, Snd)
import ElmType
import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)
import ToElm (ToElm)

import qualified Coder
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text as T
import qualified Data.Vinyl as Vinyl
import qualified Data.Vinyl.POP as POP
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

instance forall xs. (AllFieldsConstrained HasElm xs, RecordApplicative xs) =>
         HasElm (Record Identity xs) where
  hasElm = ToElm.record (recSpec (Proxy @xs))

recSpec ::
     (AllFieldsConstrained HasElm xs, RecordApplicative xs)
  => Proxy xs
  -> Record ToElm xs
recSpec _ = rpureConstrained (Proxy @HasElm) hasElm

instance forall s xs xss. ( KnownSymbol s
                          , AllFieldsAllConstrained HasElm (xs ': xss)
                          , AllFieldsConstrained RecApplicative (xs ': xss)
                          , RecordApplicative (xs ': xss)
                          , FoldRec (xs ': xss) (xs ': xss)
         ) =>
         HasElm (Label s, SOP Identity (xs ': xss)) where
  hasElm = ToElm.custom (Label @s, customSpec (Proxy @(xs ': xss)))

customSpec ::
     ( AllFieldsConstrained RecApplicative xss
     , AllFieldsAllConstrained HasElm xss
     , RecordApplicative xss
     )
  => Proxy xss
  -> POP ToElm xss
customSpec _ = POP.rpureConstrained (Proxy @HasElm) hasElm
