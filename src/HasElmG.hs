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

module HasElmG
  ( HasElmG(ElmType, elmType, to, from)
  ) where

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
  , M1(M1, unM1)
  , Meta(MetaCons, MetaData, MetaSel)
  , S
  )
import GHC.TypeLits (KnownSymbol, Symbol)
import IsElmType (IsElmType)

import qualified Data.Vinyl.Record as Record
import qualified Data.Vinyl.Sum as Sum
import qualified IsElmType

-- |
-- Generics-derived types that have some Elm equivalent type.
class HasElmG (a :: * -> *) where
  type ElmType a
  elmType :: proxy a -> IsElmType (ElmType a)
  to :: a p -> ElmType a
  from :: ElmType a -> a p

instance forall f m x xs. ( CtorList f
                          , Ctors f ~ (x ': xs)
                          , FoldRec (x ': xs) (x ': xs)
                          , KnownSymbol (Name m)
         ) =>
         HasElmG (M1 D m f) where
  type ElmType (M1 D m f) = (Label (Name m), SOP Identity (Ctors f))
  elmType _ = IsElmType.ElmCustomType Label $ elmTypeCtors (Proxy @f)
  to = (Label, ) . toCtors . unM1
  from = M1 . fromCtors . snd

-- |
-- Indicates the type is a list of Haskell constructors, part of an ADT.
class CtorList a where
  type Ctors a :: [(Symbol, [*])]
  elmTypeCtors :: proxy a -> POP IsElmType (Ctors a)
  toCtors :: a p -> SOP Identity (Ctors a)
  fromCtors :: SOP Identity (Ctors a) -> a p

instance forall named f m. ( named ~ IsNamed f
                           , ParamList' named f
                           , KnownSymbol (Name m)
         ) =>
         CtorList (M1 C m f) where
  type Ctors (M1 C m f) = '[ '( (Name m), Params' (IsNamed f) f)]
  elmTypeCtors _ =
    Record.singleton (Proxy @(Name m)) $
    elmTypeParams' (Proxy @named) (Proxy @f)
  toCtors = Sum.singleton (Proxy @(Name m)) . toParams' (Proxy @named) . unM1
  fromCtors = M1 . fromParams' (Proxy @named) . Sum.unSingleton

instance forall a b. ( Record.RecAppend (Ctors a) (Ctors b)
                     , CtorList a
                     , CtorList b
                     , Sum.CoRecAppend (Ctors a) (Ctors b)
         ) =>
         CtorList (a :+: b) where
  type Ctors (a :+: b) = Ctors a ++ Ctors b
  elmTypeCtors _ =
    Record.rappend (elmTypeCtors (Proxy @a)) (elmTypeCtors (Proxy @b))
  toCtors = Sum.cappend . bimap toCtors toCtors . toEither
  fromCtors = fromEither . bimap fromCtors fromCtors . Sum.csplit

-- |
-- Indicates the type is a parameter list for a Haskell constructor.
-- This is a wrapper around the `ParamList'` class which does the actual work.
class ParamList (a :: * -> *) where
  elmTypeParams :: Proxy a -> Record IsElmType (Params a)
  toParams :: a p -> Record Identity (Params a)
  fromParams :: Record Identity (Params a) -> a p

instance forall m f. (KnownSymbol (Name m), HasElmG f) =>
         ParamList (M1 S m f) where
  elmTypeParams _ = Record.singleton (Proxy @(Name m)) . elmType $ Proxy @f
  toParams = Record.singleton (Proxy @(Name m)) . Identity . to . unM1
  fromParams = M1 . from . getIdentity . Record.unSingleton

instance (Record.RecAppend (Params a) (Params b), ParamList a, ParamList b) =>
         ParamList (a :*: b) where
  elmTypeParams _ =
    Record.rappend (elmTypeParams $ Proxy @a) (elmTypeParams $ Proxy @b)
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
  toParams' :: Proxy n -> a p -> Rec Identity (Params' n a)
  fromParams' :: Proxy n -> Rec Identity (Params' n a) -> a p

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
  Params (a :*: b) = Params a ++ Params b
  Params (M1 S m f) = '[ '( Name m, ElmType f)]

type family IsNamed (a :: k -> *) :: Named where
  IsNamed (a :*: b) = IsNamed a
  IsNamed (M1 S ('MetaSel ('Just n) _ _ _) _) = 'Named
  IsNamed (M1 S ('MetaSel 'Nothing _ _ _) _) = 'Unnamed

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
