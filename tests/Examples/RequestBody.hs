{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module Examples.RequestBody
  ( API,
  )
where

import Data.Text (Text)
import GHC.Generics (Generic)
import Servant.API
import Servant.Interop (Rep, WIRE)

type API = "fish" :> ReqBody '[WIRE] Money :> Get '[WIRE] Fish

data Money
  = Money
      { amount :: Int,
        currency :: Text
      }
  deriving (Generic, Rep)

data Fish
  = Herring
  | Carp
  | Salmon
  deriving (Generic, Rep)
