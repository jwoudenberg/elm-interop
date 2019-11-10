{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module Golden.Void
    ( API
    ) where

import Data.Void (Void)
import Servant.Interop (WIRE)
import GHC.Generics (Generic)
import Servant.API
import qualified Wire

type API = "wish" :> Get '[ WIRE] (Either Void Unicorn)

data Unicorn
    deriving (Generic, Wire.Rep)
