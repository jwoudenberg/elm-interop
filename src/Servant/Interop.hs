{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}

-- |
-- A type class to support code generation in other languages for Servant APIs.
-- Enable code generation for an endpoint by adding `WIRE` to the list of
-- supported data formats.
--
--     "api" :> Get '[WIRE] MyType
--
-- This will require types in the `Servant` API to implement the `Rep` type
-- class, which can be derived automatically using Generics:
--
--     instance Rep MyType
module Servant.Interop
  ( NoWire,
    HasWireFormat (wireFormat),
    WIRE,
    Endpoint (..),
    Wire.Rep,
  )
where

import Data.Proxy (Proxy (Proxy))
import Data.Text (Text, pack)
import GHC.TypeLits (KnownSymbol, symbolVal)
import Network.HTTP.Media ((//))
import Network.HTTP.Types.Method (Method)
import Servant.API
import Servant.API.Modifiers (RequiredArgument)
import Servant.Server (HasServer (ServerT, hoistServerWithContext, route))
import qualified Wire

data Endpoint
  = Endpoint
      { path :: Path,
        query :: [(Text, Maybe Wire.Type_)],
        method :: Method,
        headers :: [(Text, Wire.Type_)],
        body :: Maybe Wire.Type_,
        responseBody :: Maybe Wire.Type_
      }

data Path
  = Static
      Text
      Path
  | Capture
      Wire.Type_
      Path
  | CaptureAll Wire.Type_
  | Root

data WIRE

-- | Do not generate Wire types for this endpoint.
--
--     NoWire :> "endpoint" :> Get '[JSON] Int
data NoWire

instance HasServer api c => HasServer (NoWire :> api) c where

  type ServerT (NoWire :> api) m = ServerT api m

  route _ = route (Proxy @api)

  hoistServerWithContext _ pc nt s = hoistServerWithContext (Proxy @api) pc nt s

instance Accept WIRE where
  contentType _ = "application" // "servant-interop-json"

class HasWireFormat api where
  wireFormat :: Proxy api -> (Wire.UserTypes, [Endpoint])

instance HasWireFormat (NoWire :> api) where
  wireFormat _ = (mempty, [])

instance HasWireFormat EmptyAPI where
  wireFormat _ = (mempty, [])

instance (HasWireFormat a, HasWireFormat b) => HasWireFormat (a :<|> b) where
  wireFormat _ = wireFormat (Proxy @a) <> wireFormat (Proxy @b)

instance HasWireFormat api => HasWireFormat (Description desc :> api) where
  wireFormat _ = wireFormat (Proxy @api)

instance HasWireFormat api => HasWireFormat (Summary disc :> api) where
  wireFormat _ = wireFormat (Proxy @api)

instance HasWireFormat api => HasWireFormat (HttpVersion :> api) where
  wireFormat _ = wireFormat (Proxy @api)

instance HasWireFormat api => HasWireFormat (Vault :> api) where
  wireFormat _ = wireFormat (Proxy @api)

instance HasWireFormat api => HasWireFormat (IsSecure :> api) where
  wireFormat _ = wireFormat (Proxy @api)

instance HasWireFormat api => HasWireFormat (RemoteHost :> api) where
  wireFormat _ = wireFormat (Proxy @api)

instance
  (KnownSymbol path, HasWireFormat api) =>
  HasWireFormat (path :> api)
  where
  wireFormat _ = do
    fmap addSegment <$> wireFormat (Proxy @api)
    where
      addSegment e = e {path = Static (pack $ symbolVal (Proxy @path)) (path e)}

instance
  (BoolVal (InList WIRE list), Wire.Rep a, HasWireFormat api) =>
  HasWireFormat (ReqBody' mods list a :> api)
  where
  wireFormat _ =
    if boolVal (Proxy @(InList WIRE list))
      then do
        t <- Wire.wireType (Proxy @a)
        let addBody e = e {body = Just t}
        fmap addBody <$> wireFormat (Proxy @api)
      else pure []

instance
  (KnownSymbol sym, HasWireFormat api) =>
  HasWireFormat (QueryFlag sym :> api)
  where
  wireFormat _ = fmap addQueryFlag <$> wireFormat (Proxy @api)
    where
      addQueryFlag e =
        e {query = (pack (symbolVal (Proxy @sym)), Nothing) : query e}

instance
  (KnownSymbol sym, Wire.Rep [a], HasWireFormat api) =>
  HasWireFormat (QueryParams sym a :> api)
  where
  wireFormat _ = do
    t <- Wire.wireType (Proxy @[a])
    fmap (addQueryFlag t) <$> wireFormat (Proxy @api)
    where
      addQueryFlag t e =
        e {query = (pack (symbolVal (Proxy @sym)), Just t) : query e}

instance
  ( KnownSymbol sym,
    Wire.Rep (RequiredArgument mods a),
    HasWireFormat api
  ) =>
  HasWireFormat (QueryParam' mods sym a :> api)
  where
  wireFormat _ = do
    t <- Wire.wireType (Proxy @(RequiredArgument mods a))
    fmap (addQueryFlag t) <$> wireFormat (Proxy @api)
    where
      addQueryFlag t e =
        e {query = (pack (symbolVal (Proxy @sym)), Just t) : query e}

instance
  ( KnownSymbol sym,
    Wire.Rep (RequiredArgument mods a),
    HasWireFormat api
  ) =>
  HasWireFormat (Header' mods sym a :> api)
  where
  wireFormat _ = do
    t <- Wire.wireType (Proxy @(RequiredArgument mods a))
    fmap (addHeader' t) <$> wireFormat (Proxy @api)
    where
      addHeader' t e =
        e {headers = (pack (symbolVal (Proxy @sym)), t) : headers e}

instance
  (KnownSymbol sym, Wire.Rep [t], HasWireFormat api) =>
  HasWireFormat (CaptureAll sym t :> api)
  where
  wireFormat _ = do
    t <- Wire.wireType (Proxy @[t])
    fmap (setPath t) <$> wireFormat (Proxy @api)
    where
      setPath t e = e {path = CaptureAll t}

instance
  (KnownSymbol sym, Wire.Rep t, HasWireFormat api) =>
  HasWireFormat (Capture' mods sym t :> api)
  where
  wireFormat _ = do
    t <- Wire.wireType (Proxy @[t])
    fmap (addSegment t) <$> wireFormat (Proxy @api)
    where
      addSegment t e = e {path = Capture t (path e)}

instance
  (BoolVal (InList WIRE list), Wire.Rep a, ReflectMethod method) =>
  HasWireFormat (Verb method status list a)
  where
  wireFormat _ =
    if boolVal (Proxy @(InList WIRE list))
      then do
        t <- Wire.wireType (Proxy @a)
        pure . pure $
          Endpoint
            { path = Root,
              query = [],
              method = reflectMethod (Proxy @method),
              headers = [],
              body = Nothing,
              responseBody = Just t
            }
      else pure []

instance
  (BoolVal (InList WIRE '[ct]), Wire.Rep a, ReflectMethod method) =>
  HasWireFormat (Stream method status framing ct a)
  where
  wireFormat _ =
    if boolVal (Proxy @(InList WIRE '[ct]))
      then do
        t <- Wire.wireType (Proxy @a)
        pure . pure $
          Endpoint
            { path = Root,
              query = [],
              method = reflectMethod (Proxy @method),
              headers = [],
              body = Nothing,
              responseBody = Just t
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
