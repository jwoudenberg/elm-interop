{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
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
import Data.Vinyl (Label(Label), Rec((:&), RNil))
import Data.Vinyl.CoRec (FoldRec)
import Data.Vinyl.Functor (Identity(Identity, getIdentity))
import Data.Vinyl.POP (POP)
import Data.Vinyl.Record
  ( RecAppend((+++), rappend, rsplit)
  , Record
  , singleton
  , unSingleton
  )
import Data.Vinyl.SOP (SOP)
import GHC.Generics hiding (from, to)
import GHC.TypeLits (KnownSymbol, Symbol)
import IsElmType (IsElmType(ElmRecord))

import qualified Coder
import qualified Data.Vinyl.Sum as Sum
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

class (HasElm a, ElmType a ~ Record Identity (Fields a)) =>
      HasElmRec a
  where
  type Fields a :: [(Symbol, *)]
  type Fields a = ElmFields (ElmType a)

type family ElmFields a where
  ElmFields (Record Identity xs) = xs

type family Params a where
  Params ((a :*: b) p) = Params (a p) +++ Params (b p)
  Params a = '[ ElmType a]

class HasElmParams a where
  elmTypeParams :: proxy a -> Rec IsElmType (Params a)
  toParams :: a -> Rec Identity (Params a)
  fromParams :: Rec Identity (Params a) -> a

class HasElmCtors a where
  type Ctors a :: [(Symbol, [*])]
  elmTypeCtors :: proxy a -> POP IsElmType (Ctors a)
  toCtors :: a -> SOP Identity (Ctors a)
  fromCtors :: SOP Identity (Ctors a) -> a

-- |
-- HasElm needs to support two types of product: record types and multi-param
-- constructors. The instance head of both product instances looks the same:
-- `HasElm ((a :*: b) p)`, but the constraints on `a` and `b` are that they are
-- records in one case and parameters in the other.
--
-- Records themselves can be parameters, and so for the product of two records
-- there are two instances to choose from. Because we know Haskell does not
-- allow a constructor to have multiple record params, we know the Record
-- product must take precedence here.
instance {-# OVERLAPPABLE #-} (HasElm a, Params a ~ '[ ElmType a]) =>
                              HasElmParams a where
  elmTypeParams p = elmType p :& RNil
  toParams x = Identity (to x) :& RNil
  fromParams (Identity x :& RNil) = from x

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
         HasElmRec (M1 S ('MetaSel ('Just n) su ss ds) f p)

instance {-# OVERLAPPING #-} ( RecAppend (Fields (a p)) (Fields (b p))
                             , HasElmRec (a p)
                             , HasElmRec (b p)
                             ) =>
                             HasElm ((a :*: b) p) where
  type ElmType ((a :*: b) p) = Record Identity (Fields (a p) +++ Fields (b p))
  elmType _ = elmRecAppend (elmType $ Proxy @(a p)) (elmType $ Proxy @(b p))
  to (x :*: y) = rappend (to x) (to y)
  from rec = from x :*: from y
    where
      (x, y) = rsplit rec

instance {-# OVERLAPPABLE #-} ( RecAppend (Params (a p)) (Params (b p))
                              , HasElmParams (a p)
                              , HasElmParams (b p)
                              ) =>
                              HasElmParams ((a :*: b) p) where
  elmTypeParams _ =
    rappend (elmTypeParams $ Proxy @(a p)) (elmTypeParams $ Proxy @(b p))
  toParams (x :*: y) = rappend (toParams x) (toParams y)
  fromParams params = fromParams x :*: fromParams y
    where
      (x, y) = rsplit params

instance ( RecAppend (Fields (a p)) (Fields (b p))
         , HasElmRec (a p)
         , HasElmRec (b p)
         ) =>
         HasElmRec ((a :*: b) p)

instance forall f p n fi s. (HasElmParams (f p), KnownSymbol n) =>
         HasElmCtors (M1 C ('MetaCons n fi s) f p) where
  type Ctors (M1 C ('MetaCons n fi s) f p) = '[ '( n, Params (f p))]
  elmTypeCtors _ = singleton (Proxy @n) $ elmTypeParams (Proxy @(f p))
  toCtors = Sum.singleton (Proxy @n) . toParams . unM1
  fromCtors = M1 . fromParams . Sum.unSingleton

instance forall f p n m pa nt x xs. ( HasElmCtors (f p)
                                    , Ctors (f p) ~ (x ': xs)
                                    , FoldRec (x ': xs) (x ': xs)
                                    , KnownSymbol n
         ) =>
         HasElm (M1 D ('MetaData n m pa nt) f p) where
  type ElmType (M1 D ('MetaData n m pa nt) f p) = ( Label n
                                                  , SOP Identity (Ctors (f p)))
  elmType _ = IsElmType.ElmCustomType Label $ elmTypeCtors (Proxy @(f p))
  to = (Label, ) . toCtors . unM1
  from = M1 . fromCtors . snd

elmRecAppend ::
     (RecAppend xs ys)
  => IsElmType (Record f xs)
  -> IsElmType (Record f ys)
  -> IsElmType (Record f (xs +++ ys))
elmRecAppend (ElmRecord a) (ElmRecord b) = ElmRecord (rappend a b)
