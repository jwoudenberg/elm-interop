{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}

-- |
-- A set of types that can be used in headers, query parameters, and path
-- parameters. Only primitive types and (newtype) wrappers of primitive types
-- are allowed.
module Wire.Parameter
  ( Parameter (..),
    Primitive (..),
    ParameterType (..),
  )
where

import Data.Int (Int32)
import Data.Proxy (Proxy (Proxy))
import qualified Data.Text as T
import GHC.Generics
import GHC.TypeLits (ErrorMessage (..), TypeError)
import GHC.TypeLits (KnownSymbol, symbolVal)
import qualified Wire

data Parameter
  = Parameter
      { type_ :: Primitive,
        wrapper :: Maybe (Wire.TypeName, T.Text)
      }

data Primitive
  = Int
  | String

class ParameterType a where

  parameterType :: Proxy a -> Parameter

  default parameterType :: (ParameterTypeG (Rep a)) => Proxy a -> Parameter
  parameterType _ = parameterTypeG (Proxy :: Proxy (Rep a))

instance ParameterType Int where
  parameterType _ = Parameter Int Nothing

instance ParameterType Int32 where
  parameterType _ = Parameter Int Nothing

instance ParameterType T.Text where
  parameterType _ = Parameter String Nothing

class ParameterTypeG (f :: * -> *) where
  parameterTypeG :: Proxy f -> Parameter

-- | Generics instance for a data or newtype wrapper type.
instance
  ( ParameterType k,
    KnownSymbol name,
    KnownSymbol mod,
    KnownSymbol ctor
  ) =>
  ParameterTypeG (M1 D ('MetaData name mod d4 dr) (M1 C ('MetaCons ctor c2 c3) (M1 S s1 (K1 i k))))
  where
  parameterTypeG _ =
    (parameterType (Proxy :: Proxy k))
      { wrapper =
          Just
            ( Wire.TypeName
                { Wire.typeConstructor = T.pack $ symbolVal (Proxy :: Proxy name),
                  Wire.fromModule = T.pack $ symbolVal (Proxy :: Proxy mod),
                  Wire.parameters = []
                },
              T.pack (symbolVal (Proxy :: Proxy ctor))
            )
      }

type RecordsNotAllowedMessage =
  'Text "ParameterType does not accept records."
    ':$$: 'Text "Path parameters, query parameters or headers only accept simple wrapper types."
    ':$$: 'Text ""
    ':$$: 'Text "Allowed is:"
    ':$$: 'Text ""
    ':$$: 'Text "    newtype Id = Id Int deriving (Generic)"
    ':$$: 'Text "    instance ParameterType Id"
    ':$$: 'Text ""
    ':$$: 'Text "But not accepted is:"
    ':$$: 'Text ""
    ':$$: 'Text "    data Name = Name { first :: Text, last :: Text }"
    ':$$: 'Text "      deriving (Generic)"
    ':$$: 'Text "    instance ParameterType Name"
    ':$$: 'Text ""
    ':$$: 'Text "If you need to pass complicated types to an endpoint, put them in the request body!"
    ':$$: 'Text ""

instance (TypeError RecordsNotAllowedMessage) => ParameterTypeG (D1 d1 (C1 ('MetaCons c1 c2 'True) (f :*: g))) where
  parameterTypeG _ = error "unreachable"

type MultipleParametersNotAllowedMessage =
  'Text "ParameterType only accepts use single-param constructors."
    ':$$: 'Text "Path parameters, query parameters or headers only accept simple wrapper types."
    ':$$: 'Text ""
    ':$$: 'Text "Allowed is:"
    ':$$: 'Text ""
    ':$$: 'Text "    newtype Id = Id Int deriving (Generic)"
    ':$$: 'Text "    instance ParameterType Id"
    ':$$: 'Text ""
    ':$$: 'Text "But not accepted is:"
    ':$$: 'Text ""
    ':$$: 'Text "    data Range = Range Int Int deriving (Generic)"
    ':$$: 'Text "    instance ParameterType Name"
    ':$$: 'Text ""
    ':$$: 'Text "If you need to pass complicated types to an endpoint, put them in the request body!"
    ':$$: 'Text ""

instance (TypeError MultipleParametersNotAllowedMessage) => ParameterTypeG (D1 d1 (C1 ('MetaCons c1 c2 'False) (f :*: g))) where
  parameterTypeG _ = error "unreachable"

type MultipleConstructorsNotAllowedMessage =
  'Text "ParameterType only accepts single-constructor types."
    ':$$: 'Text "Path parameters, query parameters or headers only accept simple wrapper types."
    ':$$: 'Text ""
    ':$$: 'Text "Allowed is:"
    ':$$: 'Text ""
    ':$$: 'Text "    newtype Id = Id Int deriving (Generic)"
    ':$$: 'Text "    instance ParameterType Id"
    ':$$: 'Text ""
    ':$$: 'Text "But not accepted is:"
    ':$$: 'Text ""
    ':$$: 'Text "    data Thumbs = Up | Down deriving (Generic)"
    ':$$: 'Text "    instance ParameterType Name"
    ':$$: 'Text ""
    ':$$: 'Text "If you need to pass complicated types to an endpoint, put them in the request body!"
    ':$$: 'Text ""

instance (TypeError MultipleConstructorsNotAllowedMessage) => ParameterTypeG (D1 d1 (f :+: g)) where
  parameterTypeG _ = error "unreachable"
