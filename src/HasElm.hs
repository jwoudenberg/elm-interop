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
  ( HasElmG(ElmType, elmType, to, from)
  , coder
  , typeAST
  ) where

import Data.Functor.Invariant (invmap)
import Data.Proxy (Proxy(Proxy))
import Data.Vinyl (Label(Label), Rec((:&), RNil))
import Data.Vinyl.CoRec (FoldRec)
import Data.Vinyl.Functor (Identity(Identity, getIdentity))
import Data.Vinyl.POP (POP)
import Data.Vinyl.Record (Record)
import Data.Vinyl.SOP (SOP)
import Data.Vinyl.TypeLevel (type (++))
import GHC.Generics
  ( (:*:)((:*:))
  , (:+:)(L1, R1)
  , C
  , D
  , K1(K1, unK1)
  , M1(M1, unM1)
  , Meta(MetaCons, MetaData, MetaSel)
  , S
  )
import GHC.TypeLits (KnownSymbol, Symbol)
import IsElmType (IsElmType)

import qualified Coder
import qualified Data.Vinyl.Record as Record
import qualified Data.Vinyl.Sum as Sum
import qualified ElmType
import qualified IsElmType

coder :: (HasElmG a) => proxy a -> Coder.Coder a
coder p = invmap from to . IsElmType.coder $ elmType p

typeAST :: (HasElmG a) => proxy a -> ElmType.ElmType
typeAST p = IsElmType.typeAST (elmType p)

-- |
-- Generics-derived types that have some Elm equivalent type.
class HasElmG a where
  type ElmType a
  elmType :: proxy a -> IsElmType (ElmType a)
  to :: a -> ElmType a
  from :: ElmType a -> a

instance HasElmG Int where
  type ElmType Int = Int
  elmType _ = IsElmType.ElmInt
  to = id
  from = id

instance (HasElmG a) => HasElmG (K1 i a p) where
  type ElmType (K1 i a p) = ElmType a
  elmType _ = elmType (Proxy @a)
  to = to . unK1
  from = K1 . from

instance forall f p m x xs. ( CtorList (f p)
                            , Ctors (f p) ~ (x ': xs)
                            , FoldRec (x ': xs) (x ': xs)
                            , KnownSymbol (Name m)
         ) =>
         HasElmG (M1 D m f p) where
  type ElmType (M1 D m f p) = (Label (Name m), SOP Identity (Ctors (f p)))
  elmType _ = IsElmType.ElmCustomType Label $ elmTypeCtors (Proxy @(f p))
  to = (Label, ) . toCtors . unM1
  from = M1 . fromCtors . snd

-- |
-- Indicates the type is a list of Haskell constructors, part of an ADT.
class CtorList a where
  type Ctors a :: [(Symbol, [*])]
  elmTypeCtors :: proxy a -> POP IsElmType (Ctors a)
  toCtors :: a -> SOP Identity (Ctors a)
  fromCtors :: SOP Identity (Ctors a) -> a

instance forall named f p m. ( named ~ IsNamed (f p)
                             , ParamList' named (f p)
                             , KnownSymbol (Name m)
         ) =>
         CtorList (M1 C m f p) where
  type Ctors (M1 C m f p) = '[ '( (Name m), Params' (IsNamed (f p)) (f p))]
  elmTypeCtors _ =
    Record.singleton (Proxy @(Name m)) $
    elmTypeParams' (Proxy @named) (Proxy @(f p))
  toCtors = Sum.singleton (Proxy @(Name m)) . toParams' (Proxy @named) . unM1
  fromCtors = M1 . fromParams' (Proxy @named) . Sum.unSingleton

instance forall a b p. ( Record.RecAppend (Ctors (a p)) (Ctors (b p))
                       , CtorList (a p)
                       , CtorList (b p)
                       , Sum.CoRecAppend (Ctors (a p)) (Ctors (b p))
         ) =>
         CtorList ((a :+: b) p) where
  type Ctors ((a :+: b) p) = Ctors (a p) ++ Ctors (b p)
  elmTypeCtors _ =
    Record.rappend (elmTypeCtors (Proxy @(a p))) (elmTypeCtors (Proxy @(b p)))
  toCtors = Sum.cappend . bimap toCtors toCtors . toEither
  fromCtors = fromEither . bimap fromCtors fromCtors . Sum.csplit

-- |
-- Indicates the type is a parameter list for a Haskell constructor.
-- This is a wrapper around the `ParamList'` class which does the actual work.
class ParamList (a :: *) where
  elmTypeParams :: Proxy a -> Record IsElmType (Params a)
  toParams :: a -> Record Identity (Params a)
  fromParams :: Record Identity (Params a) -> a

instance forall m f p. (KnownSymbol (Name m), HasElmG (f p)) =>
         ParamList (M1 S m f p) where
  elmTypeParams _ = Record.singleton (Proxy @(Name m)) . elmType $ Proxy @(f p)
  toParams = Record.singleton (Proxy @(Name m)) . Identity . to . unM1
  fromParams = M1 . from . getIdentity . Record.unSingleton

instance ( Record.RecAppend (Params (a p)) (Params (b p))
         , ParamList (a p)
         , ParamList (b p)
         ) =>
         ParamList ((a :*: b) p) where
  elmTypeParams _ =
    Record.rappend (elmTypeParams $ Proxy @(a p)) (elmTypeParams $ Proxy @(b p))
  toParams (x :*: y) = Record.rappend (toParams x) (toParams y)
  fromParams rec = fromParams x :*: fromParams y
    where
      (x, y) = Record.rsplit rec

-- |
-- A version of `ParamList` tagged with information about whether the included
-- parameters are named or not. Named parameters mean we're inspecting a record.
-- Unnamed parameters mean we're inspecing a parameter list.
-- Haskell requires the parameters for a single constructor to all be named or
-- all be unnamed.
class ParamList' (n :: Named) a where
  type Params' n a :: [*]
  elmTypeParams' :: Proxy n -> Proxy a -> Rec IsElmType (Params' n a)
  toParams' :: Proxy n -> a -> Rec Identity (Params' n a)
  fromParams' :: Proxy n -> Rec Identity (Params' n a) -> a

instance (ParamList a) => ParamList' 'Named a where
  type Params' 'Named a = '[ Record Identity (Params a)]
  elmTypeParams' _ p = IsElmType.ElmRecord (elmTypeParams p) :& RNil
  toParams' _ p = Identity (toParams p) :& RNil
  fromParams' _ (Identity x :& RNil) = fromParams x

instance (Record.DropFields (Params a), ParamList a) =>
         ParamList' 'Unnamed a where
  type Params' 'Unnamed a = Record.WithoutFields (Params a)
  elmTypeParams' _ = Record.dropFields . elmTypeParams
  toParams' _ = Record.dropFields . toParams
  fromParams' _ = fromParams . Record.addFields

type family Params a where
  Params ((a :*: b) p) = Params (a p) ++ Params (b p)
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

data Named
  = Named
  | Unnamed

bimap :: (a -> c) -> (b -> d) -> Either a b -> Either c d
bimap f _ (Left l) = Left (f l)
bimap _ g (Right r) = Right (g r)

toEither :: (a :+: b) p -> Either (a p) (b p)
toEither (L1 l) = Left l
toEither (R1 r) = Right r

fromEither :: Either (a p) (b p) -> (a :+: b) p
fromEither (Left l) = L1 l
fromEither (Right r) = R1 r
