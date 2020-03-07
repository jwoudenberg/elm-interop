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
    Parameter.ParameterType,
    QueryVal (..),
    Path (..),
  )
where

import Data.Functor.Foldable (Fix (Fix))
import Data.Kind (Constraint)
import qualified Data.Map.Strict as Map
import Data.Proxy (Proxy (Proxy))
import Data.Text (Text, pack)
import GHC.TypeLits (ErrorMessage (..), KnownSymbol, TypeError, symbolVal)
import Network.HTTP.Types.Method (Method)
import Servant.API
import Servant.Server (HasServer (ServerT, hoistServerWithContext, route))
import qualified Wire
import qualified Wire.Json
import qualified Wire.Parameter as Parameter

data Endpoint
  = Endpoint
      { path :: Path,
        query :: [(Text, QueryVal)],
        method :: Method,
        headers :: [(Text, Parameter.Parameter)],
        body :: Maybe Wire.Type_,
        responseBody :: Wire.Type_
      }

data QueryVal
  = QueryFlag
  | QueryParam Parameter.Parameter
  | QueryList Parameter.Parameter

data Path
  = Static
      Text
      Path
  | Capture
      Text
      Parameter.Parameter
      Path
  | CaptureAll Text Parameter.Parameter
  | Root

data WIRE

instance Accept WIRE where
  contentType _ = contentType (Proxy :: Proxy JSON)

instance Wire.Rep a => MimeRender WIRE a where
  mimeRender _ x =
    case Wire.Json.encodeJson coder (Wire.toWire x) of
      Nothing -> error "Unexpected encoding failure."
      Just encoded -> encoded
    where
      coder = Wire.Json.coderForType (Wire.wireType (Proxy :: Proxy a))

instance Wire.Rep a => MimeUnrender WIRE a where
  mimeUnrender _ x =
    case decoded of
      Nothing -> Left "Unexpected decoding failure."
      Just y -> Right y
    where
      coder = Wire.Json.coderForType (Wire.wireType (Proxy :: Proxy a))
      decoded = Wire.Json.decodeJson coder x >>= Wire.fromWire

-- | Do not generate Wire types for this endpoint.
--
--     NoWire :> "endpoint" :> Get '[JSON] Int
data NoWire

instance HasServer api c => HasServer (NoWire :> api) c where

  type ServerT (NoWire :> api) m = ServerT api m

  route _ = route (Proxy @api)

  hoistServerWithContext _ pc nt s = hoistServerWithContext (Proxy @api) pc nt s

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
  (CheckContentType list, Wire.Rep a, HasWireFormat api) =>
  HasWireFormat (ReqBody' mods list a :> api)
  where
  wireFormat _ = do
    t <- Wire.wireType (Proxy @a)
    let addBody e = e {body = Just t}
    fmap addBody <$> wireFormat (Proxy @api)

instance
  (KnownSymbol sym, HasWireFormat api) =>
  HasWireFormat (QueryFlag sym :> api)
  where
  wireFormat _ = fmap addQueryFlag <$> wireFormat (Proxy @api)
    where
      addQueryFlag e =
        e {query = (pack (symbolVal (Proxy @sym)), QueryFlag) : query e}

instance
  (KnownSymbol sym, Parameter.ParameterType a, HasWireFormat api) =>
  HasWireFormat (QueryParams sym a :> api)
  where
  wireFormat _ = do
    t <- parameterType (Proxy @a)
    fmap (addQueryFlag t) <$> wireFormat (Proxy @api)
    where
      addQueryFlag t e =
        e {query = (pack (symbolVal (Proxy @sym)), QueryList t) : query e}

instance
  ( KnownSymbol sym,
    Parameter.ParameterType a,
    HasWireFormat api
  ) =>
  HasWireFormat (QueryParam' mods sym a :> api)
  where
  wireFormat _ = do
    t <- parameterType (Proxy @a)
    fmap (addQueryFlag t) <$> wireFormat (Proxy @api)
    where
      addQueryFlag t e =
        e {query = (pack (symbolVal (Proxy @sym)), QueryParam t) : query e}

instance
  ( KnownSymbol sym,
    Parameter.ParameterType a,
    HasWireFormat api
  ) =>
  HasWireFormat (Header' mods sym a :> api)
  where
  wireFormat _ = do
    t <- parameterType (Proxy @a)
    fmap (addHeader' t) <$> wireFormat (Proxy @api)
    where
      addHeader' t e =
        e {headers = (pack (symbolVal (Proxy @sym)), t) : headers e}

instance
  (KnownSymbol sym, Parameter.ParameterType t, HasWireFormat api) =>
  HasWireFormat (CaptureAll sym t :> api)
  where
  wireFormat _ = do
    t <- parameterType (Proxy @t)
    fmap (setPath t) <$> wireFormat (Proxy @api)
    where
      setPath t e = e {path = CaptureAll (pack (symbolVal (Proxy @sym))) t}

instance
  (KnownSymbol sym, Parameter.ParameterType t, HasWireFormat api) =>
  HasWireFormat (Capture' mods sym t :> api)
  where
  wireFormat _ = do
    t <- parameterType (Proxy @t)
    fmap (addSegment t) <$> wireFormat (Proxy @api)
    where
      addSegment t e = e {path = Capture (pack (symbolVal (Proxy @sym))) t (path e)}

instance
  (CheckContentType list, Wire.Rep a, ReflectMethod method) =>
  HasWireFormat (Verb method status list a)
  where
  wireFormat _ = do
    t <- Wire.wireType (Proxy @a)
    pure . pure $
      Endpoint
        { path = Root,
          query = [],
          method = reflectMethod (Proxy @method),
          headers = [],
          body = Nothing,
          responseBody = t
        }

instance
  (CheckContentType '[ct], Wire.Rep a, ReflectMethod method) =>
  HasWireFormat (Stream method status framing ct a)
  where
  wireFormat _ = do
    t <- Wire.wireType (Proxy @a)
    pure . pure $
      Endpoint
        { path = Root,
          query = [],
          method = reflectMethod (Proxy @method),
          headers = [],
          body = Nothing,
          responseBody = t
        }

type family CheckContentType (ys :: [*]) :: Constraint where
  CheckContentType xs = (HasWIREContentType xs, NoJSONContentType xs)

-- The WIRE content type is using JSON under the hood. We cannot allow not allow
-- other types accepting JSON that might implement different JSON encoders and
-- decoders.
type JSONContentTypeNotAllowedMessage =
  'Text "You're using the JSON content-type for one of your endpoints."
    ':$$: 'Text "This is now allowed for endpoints that support code generation."
    ':$$: 'Text ""
    ':$$: 'Text "For example, if your endpoint looks like this:"
    ':$$: 'Text ""
    ':$$: 'Text "    import Servant.API ((:>), JSON)"
    ':$$: 'Text "    type Endpoint = \"api\" :> Get '[JSON] Text"
    ':$$: 'Text ""
    ':$$: 'Text "You can fix this error by using the WIRE content-type instead:"
    ':$$: 'Text ""
    ':$$: 'Text "    import Servant.API ((:>))"
    ':$$: 'Text "    import Servant.Interop (WIRE)"
    ':$$: 'Text "    type Endpoint = \"api\" :> Get '[WIRE] Text"
    ':$$: 'Text ""
    ':$$: 'Text "Alternatively you can disable code generation for the endpoint:"
    ':$$: 'Text ""
    ':$$: 'Text "    import Servant.API ((:>), JSON)"
    ':$$: 'Text "    import Servant.Interop (NoWire)"
    ':$$: 'Text "    type Endpoint = NoWire :> \"api\" :> Get '[JSON] Text"
    ':$$: 'Text ""

type family NoJSONContentType (ys :: [*]) :: Constraint where
  NoJSONContentType '[] = ()
  NoJSONContentType (JSON ': ys) = TypeError JSONContentTypeNotAllowedMessage
  NoJSONContentType (_ ': ys) = NoJSONContentType ys

type MissingWIREContentTypeMessage =
  'Text "An endpoint is missing the WIRE content-type."
    ':$$: 'Text "Ensure your servant endpoints include it, like so:"
    ':$$: 'Text ""
    ':$$: 'Text "    import Servant.API ((:>))"
    ':$$: 'Text "    import Servant.Interop (WIRE)"
    ':$$: 'Text "    type Endpoint = \"api\" :> Get '[WIRE] Text"
    ':$$: 'Text ""
    ':$$: 'Text "Alternatively you can disable code generation for the endpoint:"
    ':$$: 'Text ""
    ':$$: 'Text "    import Servant.API ((:>), PlainText)"
    ':$$: 'Text "    import Servant.Interop (NoWire)"
    ':$$: 'Text "    type Endpoint = NoWire :> \"api\" :> Get '[PlainText] Text"
    ':$$: 'Text ""

type family HasWIREContentType (ys :: [*]) :: Constraint where
  HasWIREContentType '[] = TypeError MissingWIREContentTypeMessage
  HasWIREContentType (WIRE ': ys) = ()
  HasWIREContentType (_ ': ys) = HasWIREContentType ys

parameterType :: Parameter.ParameterType a => Proxy a -> (Wire.UserTypes, Parameter.Parameter)
parameterType p =
  let parameter = Parameter.parameterType p
   in ( case Parameter.wrapper parameter of
          Nothing -> mempty
          Just (typeName, ctor) ->
            Wire.UserTypes
              $ Map.singleton typeName
              $ pure
                ( Wire.ConstructorName ctor,
                  case Parameter.type_ parameter of
                    Parameter.Int -> Fix Wire.Int
                    Parameter.String -> Fix Wire.String
                ),
        parameter
      )
