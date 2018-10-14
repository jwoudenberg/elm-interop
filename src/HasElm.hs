{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module HasElm
  ( HasElm
  , coder
  , typeAST
  ) where

import Data.Bifunctor (bimap)
import Data.Functor.Invariant (invmap)
import Data.Proxy (Proxy(Proxy))
import IsElmType (IsElmType)

import qualified Coder
import qualified ElmType
import qualified IsElmType

class HasElm a where
  type ElmType a
  isElmType :: Proxy a -> IsElmType (ElmType a)
  toElmType :: a -> ElmType a
  fromElmType :: ElmType a -> a

coder ::
     forall a. (HasElm a)
  => Coder.Coder a
coder = invmap fromElmType toElmType . IsElmType.coder $ isElmType (Proxy @a)

typeAST ::
     forall a. (HasElm a)
  => Proxy a
  -> ElmType.ElmType
typeAST = IsElmType.typeAST . isElmType

instance HasElm a => HasElm [a] where
  type ElmType [a] = [ElmType a]
  isElmType _ = IsElmType.ElmList (isElmType $ Proxy @a)
  toElmType = fmap toElmType
  fromElmType = fmap fromElmType

instance (HasElm a, HasElm b) => HasElm (a, b) where
  type ElmType (a, b) = (ElmType a, ElmType b)
  isElmType _ =
    IsElmType.ElmTuple2 (isElmType $ Proxy @a) (isElmType $ Proxy @b)
  toElmType = bimap toElmType toElmType
  fromElmType = bimap fromElmType fromElmType

instance (HasElm l, HasElm r) => HasElm (Either l r) where
  type ElmType (Either l r) = Either (ElmType l) (ElmType r)
  isElmType _ =
    IsElmType.ElmResult (isElmType $ Proxy @l) (isElmType $ Proxy @r)
  toElmType = bimap toElmType toElmType
  fromElmType = bimap fromElmType fromElmType
