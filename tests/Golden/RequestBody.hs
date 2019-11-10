{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module Golden.RequestBody
    ( API
    ) where

import Data.Text (Text)
import Servant.Interop (WIRE)
import GHC.Generics (Generic)
import Servant.API
import qualified Wire

type API = "fish" :> ReqBody '[ WIRE] Money :> Get '[ WIRE] Fish

data Money = Money
    { amount :: Int
    , currency :: Text
    } deriving (Generic, Wire.Rep)

data Fish
    = Herring
    | Carp
    | Salmon
    deriving (Generic, Wire.Rep)
