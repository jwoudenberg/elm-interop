{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
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
  ( DropFields(WithoutFields, addFields, dropFields)
  , RecAppend((+++), rappend, rsplit)
  , Record
  , singleton
  , unSingleton
  )
import Data.Vinyl.SOP (SOP)
import GHC.Generics hiding (from, to)
import GHC.TypeLits (KnownSymbol, Symbol)
import IsElmType (IsElmType)

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

type family Params a where
  Params ((a :*: b) p) = Params (a p) +++ Params (b p)
  Params (M1 S ('MetaSel mn su ss ds) f p) = '[ '( Name mn, ElmType (f p))]

type family IsNamed a where
  IsNamed ((a :*: b) p) = IsNamed (a p)
  IsNamed (M1 S ('MetaSel mn su ss ds) f p) = HasName mn

type family Name (a :: Maybe Symbol) where
  Name ('Just n) = n
  Name 'Nothing = ""

type family HasName (a :: Maybe Symbol) where
  HasName ('Just n) = 'Named
  HasName 'Nothing = 'Unnamed

class HasParams' (n :: Named) a where
  type Params' n a :: [*]
  elmTypeParams' :: Proxy n -> Proxy a -> Rec IsElmType (Params' n a)
  toParams' :: Proxy n -> a -> Rec Identity (Params' n a)
  fromParams' :: Proxy n -> Rec Identity (Params' n a) -> a

instance (HasParams a) => HasParams' 'Named a where
  type Params' 'Named a = '[ Record Identity (Params a)]
  elmTypeParams' _ p = IsElmType.ElmRecord (elmTypeParams p) :& RNil
  toParams' _ p = Identity (toParams p) :& RNil
  fromParams' _ (Identity x :& RNil) = fromParams x

instance (DropFields (Params a), HasParams a) => HasParams' 'Unnamed a where
  type Params' 'Unnamed a = WithoutFields (Params a)
  elmTypeParams' _ = dropFields . elmTypeParams
  toParams' _ = dropFields . toParams
  fromParams' _ = fromParams . addFields

class HasParams (a :: *) where
  elmTypeParams :: Proxy a -> Record IsElmType (Params a)
  toParams :: a -> Record Identity (Params a)
  fromParams :: Record Identity (Params a) -> a

data Named
  = Named
  | Unnamed

class HasElmCtors a where
  type Ctors a :: [(Symbol, [*])]
  elmTypeCtors :: proxy a -> POP IsElmType (Ctors a)
  toCtors :: a -> SOP Identity (Ctors a)
  fromCtors :: SOP Identity (Ctors a) -> a

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

instance forall mn su ss ds f p. (KnownSymbol (Name mn), HasElm (f p)) =>
         HasParams (M1 S ('MetaSel mn su ss ds) f p) where
  elmTypeParams _ = singleton (Proxy @(Name mn)) . elmType $ Proxy @(f p)
  toParams = singleton (Proxy @(Name mn)) . Identity . to . unM1
  fromParams = M1 . from . getIdentity . unSingleton

instance ( RecAppend (Params (a p)) (Params (b p))
         , HasParams (a p)
         , HasParams (b p)
         ) =>
         HasParams ((a :*: b) p) where
  elmTypeParams _ =
    rappend (elmTypeParams $ Proxy @(a p)) (elmTypeParams $ Proxy @(b p))
  toParams (x :*: y) = rappend (toParams x) (toParams y)
  fromParams rec = fromParams x :*: fromParams y
    where
      (x, y) = rsplit rec

instance forall named f p n fi s. ( named ~ IsNamed (f p)
                                  , HasParams' named (f p)
                                  , KnownSymbol n
         ) =>
         HasElmCtors (M1 C ('MetaCons n fi s) f p) where
  type Ctors (M1 C ('MetaCons n fi s) f p) = '[ '( n, Params' (IsNamed (f p)) (f p))]
  elmTypeCtors _ =
    singleton (Proxy @n) $ elmTypeParams' (Proxy @named) (Proxy @(f p))
  toCtors = Sum.singleton (Proxy @n) . toParams' (Proxy @named) . unM1
  fromCtors = M1 . fromParams' (Proxy @named) . Sum.unSingleton

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
