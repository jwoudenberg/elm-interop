{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module Examples.Roundtrip
  ( API,
  )
where

import Data.Text (Text)
import GHC.Generics (Generic)
import Servant.API
import Servant.Interop (Rep, WIRE)

type API =
  GetValue :<|> ReturnValue

type GetValue =
  "roundtrip" :> Get '[WIRE] Value

type ReturnValue =
  "roundtrip"
    :> ReqBody '[WIRE] (Either Text Value)
    :> Post '[WIRE] Value

newtype Value = Value Int
  deriving (Generic)

instance Rep Value
