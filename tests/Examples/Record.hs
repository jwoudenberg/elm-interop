{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module Examples.Record
  ( API,
  )
where

import Data.Text (Text)
import GHC.Generics (Generic)
import Servant.API
import Servant.Interop (Rep, WIRE)

type API = "socks" :> Get '[WIRE] Sock

data Sock
  = Sock
      { color :: Text,
        pattern :: Pattern,
        holes :: Int
      }
  deriving (Generic)

instance Rep Sock

data Pattern
  = None
  | Stripes
  | Dots
  | Other
  deriving (Generic)

instance Rep Pattern
