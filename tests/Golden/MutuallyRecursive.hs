{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module Golden.MutuallyRecursive
    ( API
    ) where

import Data.Text (Text)
import Servant.Elm (ELM)
import GHC.Generics (Generic)
import Servant.API
import qualified Wire

type API = "duet" :> Get '[ ELM] (BackAndForth Line)

type Line = Text

data BackAndForth a =
    Back a
         (Forth a)
    deriving (Generic, Wire.Rep)

data Forth a =
    Forth a
          (BackAndForth a)
    deriving (Generic, Wire.Rep)
