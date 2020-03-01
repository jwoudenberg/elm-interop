{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module Examples.MutuallyRecursive
  ( API,
  )
where

import Data.Text (Text)
import GHC.Generics (Generic)
import Servant.API
import Servant.Interop (Rep, WIRE)

type API = "duet" :> Get '[WIRE] (BackAndForth Line)

type Line = Text

data BackAndForth a
  = Back
      a
      (Forth a)
  deriving (Generic)

instance Rep a => Rep (BackAndForth a)

data Forth a
  = Forth
      a
      (BackAndForth a)
  deriving (Generic)

instance Rep a => Rep (Forth a)
