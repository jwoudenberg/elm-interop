{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}

module Elm.Servant
  ( HasElm
  , ELM
  , elmTypes
  ) where

import Data.Proxy (Proxy(Proxy))
import Data.Text (Text, pack)
import Elm
import GHC.TypeLits (KnownSymbol, symbolVal)
import Network.HTTP.Media ((//))
import Network.HTTP.Types.Method (Method)
import Servant.API
import Servant.API.Modifiers (RequiredArgument)

data Endpoint = Endpoint
  { path :: Path
  , query :: [(Text, Maybe ElmType)]
  , method :: Method
  , headers :: [(Text, ElmType)]
  , body :: Maybe ElmType
  , responseBody :: Maybe ElmType
  }

elmTypes :: HasElm api => Proxy api -> UserTypes
elmTypes = fst . hasElm

data Path
  = Static Text
           Path
  | Capture ElmType
            Path
  | CaptureAll ElmType
  | Root

data ELM

instance Accept ELM where
  contentType _ = "application" // "elm-interop-json"

class HasElm api where
  hasElm :: Proxy api -> (UserTypes, [Endpoint])

instance HasElm EmptyAPI where
  hasElm _ = (mempty, [])

instance (HasElm a, HasElm b) => HasElm (a :<|> b) where
  hasElm _ = hasElm (Proxy @a) <> hasElm (Proxy @b)

instance HasElm api => HasElm (Description desc :> api) where
  hasElm _ = hasElm (Proxy @api)

instance HasElm api => HasElm (Summary disc :> api) where
  hasElm _ = hasElm (Proxy @api)

instance HasElm api => HasElm (HttpVersion :> api) where
  hasElm _ = hasElm (Proxy @api)

instance HasElm api => HasElm (Vault :> api) where
  hasElm _ = hasElm (Proxy @api)

instance HasElm api => HasElm (IsSecure :> api) where
  hasElm _ = hasElm (Proxy @api)

instance HasElm api => HasElm (RemoteHost :> api) where
  hasElm _ = hasElm (Proxy @api)

instance (KnownSymbol path, HasElm api) => HasElm (path :> api) where
  hasElm _ = do
    fmap addSegment <$> hasElm (Proxy @api)
    where
      addSegment e = e {path = Static (pack $ symbolVal (Proxy @path)) (path e)}

instance (BoolVal (InList ELM list), Elm a, HasElm api) =>
         HasElm (ReqBody' mods list a :> api) where
  hasElm _ =
    if boolVal (Proxy @(InList ELM list))
      then do
        t <- elmType (Proxy @a)
        let addBody e = e {body = Just t}
        fmap addBody <$> hasElm (Proxy @api)
      else pure []

instance (KnownSymbol sym, HasElm api) => HasElm (QueryFlag sym :> api) where
  hasElm _ = fmap addQueryFlag <$> hasElm (Proxy @api)
    where
      addQueryFlag e =
        e {query = (pack (symbolVal (Proxy @sym)), Nothing) : query e}

instance (KnownSymbol sym, Elm [a], HasElm api) =>
         HasElm (QueryParams sym a :> api) where
  hasElm _ = do
    t <- elmType (Proxy @[a])
    fmap (addQueryFlag t) <$> hasElm (Proxy @api)
    where
      addQueryFlag t e =
        e {query = (pack (symbolVal (Proxy @sym)), Just t) : query e}

instance (KnownSymbol sym, Elm (RequiredArgument mods a), HasElm api) =>
         HasElm (QueryParam' mods sym a :> api) where
  hasElm _ = do
    t <- elmType (Proxy @(RequiredArgument mods a))
    fmap (addQueryFlag t) <$> hasElm (Proxy @api)
    where
      addQueryFlag t e =
        e {query = (pack (symbolVal (Proxy @sym)), Just t) : query e}

instance (KnownSymbol sym, Elm (RequiredArgument mods a), HasElm api) =>
         HasElm (Header' mods sym a :> api) where
  hasElm _ = do
    t <- elmType (Proxy @(RequiredArgument mods a))
    fmap (addHeader' t) <$> hasElm (Proxy @api)
    where
      addHeader' t e =
        e {headers = (pack (symbolVal (Proxy @sym)), t) : headers e}

instance (KnownSymbol sym, Elm [t], HasElm api) =>
         HasElm (CaptureAll sym t :> api) where
  hasElm _ = do
    t <- elmType (Proxy @[t])
    fmap (setPath t) <$> hasElm (Proxy @api)
    where
      setPath t e = e {path = CaptureAll t}

instance (KnownSymbol sym, Elm t, HasElm api) =>
         HasElm (Capture' mods sym t :> api) where
  hasElm _ = do
    t <- elmType (Proxy @[t])
    fmap (addSegment t) <$> hasElm (Proxy @api)
    where
      addSegment t e = e {path = Capture t (path e)}

instance (BoolVal (InList ELM list), Elm a, ReflectMethod method) =>
         HasElm (Verb method status list a) where
  hasElm _ =
    if boolVal (Proxy @(InList ELM list))
      then do
        t <- elmType (Proxy @a)
        pure . pure $
          Endpoint
            { path = Root
            , query = []
            , method = reflectMethod (Proxy @method)
            , headers = []
            , body = Nothing
            , responseBody = Just t
            }
      else pure []

instance (BoolVal (InList ELM '[ ct]), Elm a, ReflectMethod method) =>
         HasElm (Stream method status framing ct a) where
  hasElm _ =
    if boolVal (Proxy @(InList ELM '[ ct]))
      then do
        t <- elmType (Proxy @a)
        pure . pure $
          Endpoint
            { path = Root
            , query = []
            , method = reflectMethod (Proxy @method)
            , headers = []
            , body = Nothing
            , responseBody = Just t
            }
      else pure []

type family InList (x :: *) (ys :: [*]) :: Bool where
  InList x '[] = 'False
  InList x (x ': ys) = 'True
  InList x (y ': ys) = InList x ys

class BoolVal (bool :: Bool) where
  boolVal :: Proxy bool -> Bool

instance BoolVal 'True where
  boolVal _ = True

instance BoolVal 'False where
  boolVal _ = False
