{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module Examples.RequestHeaders
  ( API,
  )
where

import qualified Data.Text as T
import GHC.Generics (Generic)
import Servant.API
import Servant.Interop (ParameterType, Rep, WIRE)

type API =
  "secret"
    :> Header "password" Password
    :> Header "fingerprint" Double
    :> Header "voiceprint" Double
    :> Get '[WIRE] Secret

newtype Password = Password T.Text
  deriving (Generic)

instance ParameterType Password

newtype Secret = Secret T.Text
  deriving (Generic)

instance Rep Secret
