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

import Coder (Coder, WrappedField(WrappedField), (=:), handle, match)
import Data.Functor.Foldable (Fix(Fix))
import Data.Functor.Invariant (Invariant(invmap))
import Data.HashMap.Strict (HashMap)
import Data.Proxy (Proxy(Proxy))
import Data.Vinyl
  ( AllFields
  , Dict(Dict)
  , ElField(Field)
  , HList
  , KnownField
  , Label(Label)
  , Rec((:&), RNil)
  , RecApplicative
  , reifyConstraint
  , rfoldMap
  , rmap
  , rmapf
  , rpure
  , rpureConstrained
  , rpuref
  )
import Data.Vinyl.CoRec (CoRec, FoldRec)
import Data.Vinyl.Functor ((:.), Compose(Compose), Identity)
import Data.Vinyl.TypeLevel (AllConstrained, Fst, RecAll, Snd)
import ElmSOP ()
import ElmType
import GHC.Exts (Constraint)
import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)
import ToElm (ToElm(ToElm, coder, elmType))

import qualified Coder
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text as T
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

instance (HasElm a, KnownSymbol s) =>
         HasElmField (WrappedField Proxy '( s, a)) where
  type FieldElmType (WrappedField Proxy '( s, a)) = a
  hasElmField (WrappedField x) = mkToElm x

instance forall xs. ( RecAll (WrappedField Proxy) xs HasElmField
                    , RecApplicative xs
                    , AllFields xs
         ) =>
         HasElm (Rec ElField xs) where
  hasElm =
    ToElm
      { coder = Coder.record $ codersRec (Proxy @xs)
      , elmType = Fix . ElmRecord . rfoldMap toElmField $ toElms (Proxy @xs)
      }

codersSOP ::
     (RecAll (WrappedField Proxy) xs HasElmField, AllFields xs)
  => Proxy xs
  -> Rec (WrappedField (Coder :. HList)) xs
codersSOP p = undefined

codersRec ::
     (RecAll (WrappedField Proxy) xs HasElmField, AllFields xs)
  => Proxy xs
  -> Rec (WrappedField Coder) xs
codersRec p = rmapf (\(WrappedField x) -> (WrappedField $ coder x)) (toElms p)

toElmSOP ::
     (RecAll (WrappedField Proxy) xs HasElmField, AllFields xs)
  => Proxy xs
  -> Rec (WrappedField (Rec ToElm)) xs
toElmSOP _ = undefined

toElms ::
     (RecAll (WrappedField Proxy) xs HasElmField, AllFields xs)
  => Proxy xs
  -> Rec (WrappedField ToElm) xs
toElms _ = rmapf getToElm hasElmFieldRec
  where
    getToElm ::
         (KnownSymbol s)
      => (Dict HasElmField :. WrappedField Proxy) '( s, t)
      -> WrappedField ToElm '( s, t)
    getToElm (Compose (Dict x)) = WrappedField (hasElmField x)

hasElmFieldRec ::
     (RecAll (WrappedField Proxy) xs HasElmField, AllFields xs)
  => Rec (Dict HasElmField :. WrappedField Proxy) xs
hasElmFieldRec = reifyConstraint (Proxy :: Proxy HasElmField) proxyRec

class InRecApplicative f

instance (RecApplicative xs) => InRecApplicative (f xs)

-- -- | Constraint that all types in a type-level list satisfy a
-- -- constraint.
-- type family AllFieldsConstrained c ts :: Constraint where
--   AllFieldsConstrained c '[] = ()
--   AllFieldsConstrained c (t ': ts) = (c (Snd t), AllFieldsConstrained c ts)
proxySOP ::
     (AllFields xs)
  => Rec (WrappedField (Dict InRecApplicative :. Rec Proxy)) xs
proxySOP = rpuref (WrappedField proxyRec')

proxyRec' :: (Dict InRecApplicative :. Rec Proxy) xs
proxyRec' = undefined

proxyRec :: (AllFields xs) => Rec (WrappedField Proxy) xs
proxyRec = rpuref (WrappedField Proxy)

toElmField :: WrappedField ToElm a -> HashMap T.Text ElmType
toElmField t@(WrappedField toElm) =
  HashMap.singleton (Coder.getLabel t) (elmType toElm)

mkToElm :: (HasElm a) => proxy a -> ToElm a
mkToElm _ = hasElm

type SOP f xs = CoRec (WrappedField (Rec f)) xs

instance forall s x xs. ( KnownSymbol s
                        , RecAll (WrappedField Proxy) (x ': xs) HasElmField
                        , RecApplicative (x ': xs)
                        , AllFields (x ': xs)
                        , FoldRec (x ': xs) (x ': xs)
         ) =>
         HasElm (Label s, CoRec (WrappedField HList) (x ': xs)) where
  hasElm =
    ToElm
      { coder =
          invmap (Label, ) snd . Coder.union $ codersSOP (Proxy @(x ': xs))
      , elmType =
          Fix .
          ElmCustomType (T.pack . symbolVal $ Proxy @s) .
          rfoldMap toElmConstructor $
          toElmSOP (Proxy @(x ': xs))
      }

toElmConstructor :: WrappedField (Rec ToElm) a -> HashMap T.Text [ElmType]
toElmConstructor t@(WrappedField toElm) =
  HashMap.singleton (Coder.getLabel t) (toElmParameter toElm)

toElmParameter :: Rec ToElm a -> [ElmType]
toElmParameter = rfoldMap (pure . elmType)
