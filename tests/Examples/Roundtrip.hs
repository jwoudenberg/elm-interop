{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module Examples.Roundtrip
  ( API,
    server,
    Settings (..),
    Value (..),
  )
where

import qualified Control.Concurrent.MVar as MVar
import Control.Monad.IO.Class (liftIO)
import GHC.Generics (Generic)
import Servant.API
import Servant.Interop (Rep, WIRE)
import qualified Servant.Server

type API =
  GetValue :<|> ReturnValue

type GetValue =
  "roundtrip" :> Get '[WIRE] Value

type ReturnValue =
  "roundtrip"
    :> ReqBody '[WIRE] Value
    :> Post '[WIRE] NoContent

newtype Value = Value Int
  deriving (Generic, Show)

instance Rep Value

data Settings
  = Settings
      { servedValue :: MVar.MVar Value,
        receivedValue :: MVar.MVar Value
      }

server :: Settings -> Servant.Server.Server API
server settings =
  getValue settings :<|> returnValue settings

getValue :: Settings -> Servant.Server.Handler Value
getValue = liftIO . MVar.readMVar . servedValue

returnValue :: Settings -> Value -> Servant.Server.Handler NoContent
returnValue settings value = do
  liftIO $ MVar.putMVar (receivedValue settings) value
  pure NoContent
