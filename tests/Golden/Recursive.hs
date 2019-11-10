{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module Golden.Recursive
    ( API
    ) where

import Data.Text (Text)
import Elm.Servant (ELM)
import GHC.Generics (Generic)
import Servant.API
import qualified Wire

type API = "turtles" :> Get '[ ELM] Turtle

data Turtle = Turtle
    { name :: Text
    , onBackOf :: Turtle
    } deriving (Generic, Wire.Rep)
