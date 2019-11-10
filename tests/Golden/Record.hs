{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module Golden.Record
  ( API
  ) where

import Data.Text (Text)
import Servant.Elm (WIRE)
import GHC.Generics (Generic)
import Servant.API
import qualified Wire

type API = "socks" :> Get '[ WIRE] Sock

data Sock = Sock
  { color :: Text
  , pattern :: Pattern
  , holes :: Int
  } deriving (Generic, Wire.Rep)

data Pattern
  = None
  | Stripes
  | Dots
  | Other
  deriving (Generic, Wire.Rep)
