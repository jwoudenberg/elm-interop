{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module HasElm
  ( HasElm(ElmType, elmType, to, from)
  , coder
  , typeAST
  ) where

import Data.Functor.Invariant (invmap)
import Data.Proxy (Proxy(Proxy))
import Data.Vinyl.Functor (Identity(Identity, getIdentity))
import Data.Vinyl.Record
  ( RecAppend((+++), rappend, rsplit)
  , Record
  , singleton
  , unSingleton
  )
import GHC.Generics hiding (from, to)
import GHC.TypeLits (KnownSymbol, Symbol)
import IsElmType (IsElmType(ElmRecord))

import qualified Coder
import qualified ElmType
import qualified IsElmType

coder :: (HasElm a) => proxy a -> Coder.Coder a
coder p = invmap from to . IsElmType.coder $ elmType p

typeAST :: (HasElm a) => proxy a -> ElmType.ElmType
typeAST p = IsElmType.typeAST (elmType p)

class HasElm a where
  type ElmType a
  elmType :: proxy a -> IsElmType (ElmType a)
  to :: a -> ElmType a
  from :: ElmType a -> a

class (HasElm a, ElmType a ~ Record Identity (Fields' a)) =>
      HasElmRec' a
  where
  type Fields' a :: [(Symbol, *)]
  type Fields' a = ElmFields (ElmType a)

type family ElmFields a where
  ElmFields (Record Identity xs) = xs

instance HasElm Int where
  type ElmType Int = Int
  elmType _ = IsElmType.ElmInt
  to = id
  from = id

instance (HasElm a) => HasElm (K1 i a p) where
  type ElmType (K1 i a p) = ElmType a
  elmType _ = elmType (Proxy @a)
  to = to . unK1
  from = K1 . from

instance forall n su ss ds f p. (KnownSymbol n, HasElm (f p)) =>
         HasElm (M1 S ('MetaSel ('Just n) su ss ds) f p) where
  type ElmType (M1 S ('MetaSel ('Just n) su ss ds) f p) = Record Identity '[ '( n, ElmType (f p))]
  elmType _ = ElmRecord . singleton (Proxy @n) . elmType $ Proxy @(f p)
  to = singleton (Proxy @n) . Identity . to . unM1
  from = M1 . from . getIdentity . unSingleton

instance forall n su ss ds f p. (KnownSymbol n, HasElm (f p)) =>
         HasElmRec' (M1 S ('MetaSel ('Just n) su ss ds) f p)

instance ( RecAppend (Fields' (a p)) (Fields' (b p))
         , HasElmRec' (a p)
         , HasElmRec' (b p)
         ) =>
         HasElm ((a :*: b) p) where
  type ElmType ((a :*: b) p) = Record Identity (Fields' (a p) +++ Fields' (b p))
  elmType _ = elmRecAppend (elmType $ Proxy @(a p)) (elmType $ Proxy @(b p))
  to (x :*: y) = rappend (to x) (to y)
  from rec = from x :*: from y
    where
      (x, y) = rsplit rec

instance ( RecAppend (Fields' (a p)) (Fields' (b p))
         , HasElmRec' (a p)
         , HasElmRec' (b p)
         ) =>
         HasElmRec' ((a :*: b) p)

-- | TODO: wrap in constructor
instance forall f p n fi s. (HasElm (f p)) =>
         HasElm (M1 C ('MetaCons n fi s) f p) where
  type ElmType (M1 C ('MetaCons n fi s) f p) = ElmType (f p)
  elmType _ = elmType (Proxy @(f p))
  to = to . unM1
  from = M1 . from

-- | TODO: wrap in type name
instance forall f p c. (HasElm (f p)) => HasElm (M1 D c f p) where
  type ElmType (M1 D c f p) = ElmType (f p)
  elmType _ = elmType (Proxy @(f p))
  to = to . unM1
  from = M1 . from

elmRecAppend ::
     (RecAppend xs ys)
  => IsElmType (Record f xs)
  -> IsElmType (Record f ys)
  -> IsElmType (Record f (xs +++ ys))
elmRecAppend (ElmRecord a) (ElmRecord b) = ElmRecord (rappend a b)
