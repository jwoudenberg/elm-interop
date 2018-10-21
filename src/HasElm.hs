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
  Params (M1 S m f p) = '[ '( Name m, ElmType (f p))]

type family IsNamed a :: Named where
  IsNamed ((a :*: b) p) = IsNamed (a p)
  IsNamed (M1 S ('MetaSel ('Just n) _ _ _) _ _) = 'Named
  IsNamed (M1 S ('MetaSel 'Nothing _ _ _) _ _) = 'Unnamed

type family Name (a :: Meta) :: Symbol where
  Name ('MetaSel ('Just n) _ _ _) = n
  Name ('MetaSel 'Nothing _ _ _) = ""
  Name ('MetaCons n _ _) = n
  Name ('MetaData n _ _ _) = n

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

instance forall m f p. (KnownSymbol (Name m), HasElm (f p)) =>
         HasParams (M1 S m f p) where
  elmTypeParams _ = singleton (Proxy @(Name m)) . elmType $ Proxy @(f p)
  toParams = singleton (Proxy @(Name m)) . Identity . to . unM1
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

instance forall named f p m. ( named ~ IsNamed (f p)
                             , HasParams' named (f p)
                             , KnownSymbol (Name m)
         ) =>
         HasElmCtors (M1 C m f p) where
  type Ctors (M1 C m f p) = '[ '( (Name m), Params' (IsNamed (f p)) (f p))]
  elmTypeCtors _ =
    singleton (Proxy @(Name m)) $ elmTypeParams' (Proxy @named) (Proxy @(f p))
  toCtors = Sum.singleton (Proxy @(Name m)) . toParams' (Proxy @named) . unM1
  fromCtors = M1 . fromParams' (Proxy @named) . Sum.unSingleton

instance forall f p m x xs. ( HasElmCtors (f p)
                            , Ctors (f p) ~ (x ': xs)
                            , FoldRec (x ': xs) (x ': xs)
                            , KnownSymbol (Name m)
         ) =>
         HasElm (M1 D m f p) where
  type ElmType (M1 D m f p) = (Label (Name m), SOP Identity (Ctors (f p)))
  elmType _ = IsElmType.ElmCustomType Label $ elmTypeCtors (Proxy @(f p))
  to = (Label, ) . toCtors . unM1
  from = M1 . fromCtors . snd
