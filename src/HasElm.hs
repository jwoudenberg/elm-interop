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
  , HasElmRec(toRec, fromRec)
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

instance (HasElm a) => HasElm (K1 i a p) where
  type ElmType (K1 i a p) = ElmType a
  elmType _ = elmType (Proxy @a)
  to = to . unK1
  from = K1 . from

class HasElmRec a where
  type Fields a :: [(Symbol, *)]
  elmTypeRec :: proxy a -> IsElmType (Record Identity (Fields a))
  toRec :: a -> Record Identity (Fields a)
  fromRec :: Record Identity (Fields a) -> a

instance forall n su ss ds f p. (KnownSymbol n, HasElm (f p)) =>
         HasElmRec (M1 S ('MetaSel ('Just n) su ss ds) f p) where
  type Fields (M1 S ('MetaSel ('Just n) su ss ds) f p) = '[ '( n, ElmType (f p))]
  elmTypeRec _ = ElmRecord . singleton (Proxy @n) . elmType $ Proxy @(f p)
  toRec = singleton (Proxy @n) . Identity . to . unM1
  fromRec = M1 . from . getIdentity . unSingleton

instance ( RecAppend (Fields (a p)) (Fields (b p))
         , HasElmRec (a p)
         , HasElmRec (b p)
         ) =>
         HasElmRec ((a :*: b) p) where
  type Fields ((a :*: b) p) = Fields (a p) +++ Fields (b p)
  elmTypeRec _ =
    elmRecAppend (elmTypeRec $ Proxy @(a p)) (elmTypeRec $ Proxy @(b p))
  toRec (x :*: y) = rappend (toRec x) (toRec y)
  fromRec rec = fromRec x :*: fromRec y
    where
      (x, y) = rsplit rec

elmRecAppend ::
     (RecAppend xs ys)
  => IsElmType (Record f xs)
  -> IsElmType (Record f ys)
  -> IsElmType (Record f (xs +++ ys))
elmRecAppend (ElmRecord a) (ElmRecord b) = ElmRecord (rappend a b)
