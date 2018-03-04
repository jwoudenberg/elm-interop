{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module ElmType where

import Data.Text
import qualified ElmType.TypeAST as AST
import GHC.TypeLits
import Generics.SOP
import Named

class ElmType a where
  astForType :: Proxy a -> AST.AST

instance ElmType Int where
  astForType _ = AST.Int

instance ElmType Text where
  astForType _ = AST.String

instance (All ElmField xs) => ElmType (NP I xs) where
  astForType =
    AST.Record . hcfoldMap (Proxy @ElmField) (pure . field) . fromNPProxy

instance (All ElmConstructor xs, KnownSymbol s) =>
         ElmType (Named s (NS I xs)) where
  astForType x = AST.Union (Named.name x) (constructors $ unNamed <$> x)
    where
      constructors :: Proxy (NS I xs) -> [(Text, [AST.AST])]
      constructors =
        hcfoldMap (Proxy @ElmConstructor) (pure . constructor) . fromNSProxy

constructor ::
     forall a. ElmConstructor a
  => Proxy a
  -> (Text, [AST.AST])
constructor x =
  ( ElmType.name x
  , hcfoldMap (Proxy @ElmType) (pure . astForType) . fromNPProxy $
    Proxy @(NP I (Params a)))

field ::
     forall f. ElmField f
  => Proxy f
  -> (Text, AST.AST)
field _ = (key (Proxy @f), astForType (Proxy @(Value f)))

fromNPProxy :: SListI xs => Proxy (NP I xs) -> NP Proxy xs
fromNPProxy _ = hpure Proxy

fromNSProxy :: SListI xs => Proxy (NS I xs) -> NP Proxy xs
fromNSProxy _ = hpure Proxy

class ElmType (Value a) =>
      ElmField a where
  type Value a :: *
  key :: Proxy a -> Text

instance (KnownSymbol s, ElmType t) => ElmField (Named s t) where
  type Value (Named s t) = t
  key = Named.name

class (All ElmType (Params a)) =>
      ElmConstructor a where
  type Params a :: [*]
  name :: Proxy a -> Text

instance (All ElmType xs, KnownSymbol s) =>
         ElmConstructor (Named s (NP I xs)) where
  type Params (Named s (NP I xs)) = xs
  name = Named.name
