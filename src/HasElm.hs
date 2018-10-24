{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

module HasElm
  ( HasElm
  , coder
  , typeAST
  ) where

import Data.Functor.Invariant (invmap)
import Data.Proxy (Proxy(Proxy))
import GHC.Generics (Generic, Rep)
import IsElmType (IsElmType)

import qualified Coder
import qualified ElmType
import qualified GHC.Generics as Generics
import qualified HasElmG
import qualified IsElmType

type family J a where
  J a = (ElmType a ~ HasElmG.ElmType (Rep a))

-- |
-- Generics-derived types that have some Elm equivalent type.
class HasElm a where
  type ElmType a
  elmType :: Proxy a -> IsElmType (ElmType a)
  to :: a -> ElmType a
  from :: ElmType a -> a
  type ElmType a = HasElmG.ElmType (Rep a)
  default elmType :: (Generic a, HasElmG.HasElmG (Rep a), J a) =>
    Proxy a -> IsElmType (ElmType a)
  elmType p = HasElmG.elmType (proxyRep p)
  default to :: (Generic a, HasElmG.HasElmG (Rep a), J a) =>
    a -> ElmType a
  to = HasElmG.to . Generics.from
  default from :: (J a, Generic a, HasElmG.HasElmG (Rep a)) =>
    ElmType a -> a
  from = Generics.to . HasElmG.from

proxyRep :: Proxy a -> Proxy (Rep a :: * -> *)
proxyRep Proxy = Proxy

instance HasElm Int where
  type ElmType Int = Int
  elmType _ = IsElmType.ElmInt
  to = id
  from = id

coder :: (HasElm a) => Proxy a -> Coder.Coder a
coder p = invmap from to . IsElmType.coder $ elmType p

typeAST :: (HasElm a) => Proxy a -> ElmType.ElmType
typeAST p = IsElmType.typeAST (elmType p)
