{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module Examples.Void
  ( API,
  )
where

import Data.Void (Void)
import GHC.Generics (Generic)
import Servant.API
import Servant.Interop (Rep, WIRE)

type API = "wish" :> Get '[WIRE] (Either Void Unicorn)

data Unicorn
  deriving (Generic)

instance Rep Unicorn
