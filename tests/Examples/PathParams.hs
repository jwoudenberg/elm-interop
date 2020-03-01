{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module Examples.PathParams
  ( API,
  )
where

import qualified Data.Text as T
import GHC.Generics (Generic)
import Servant.API
import Servant.Interop (ParameterType, Rep, WIRE)

type API =
  "route"
    :> Capture "start" City
    :> CaptureAll "stop" City
    :> Get '[WIRE] Kilometers

newtype City = City T.Text
  deriving (Generic)

instance ParameterType City

newtype Kilometers = Kilometers Int
  deriving (Generic)

instance Rep Kilometers
