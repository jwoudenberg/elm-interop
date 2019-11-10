{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module Golden.Record
  ( API
  ) where

import Data.Text (Text)
import Elm.Servant (ELM)
import GHC.Generics (Generic)
import Servant.API
import qualified Wire

type API = "socks" :> Get '[ ELM] Sock

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
