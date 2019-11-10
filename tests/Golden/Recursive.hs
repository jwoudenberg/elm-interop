{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module Golden.Recursive
    ( API
    ) where

import Data.Text (Text)
import Servant.Elm (WIRE)
import GHC.Generics (Generic)
import Servant.API
import qualified Wire

type API = "turtles" :> Get '[ WIRE] Turtle

data Turtle = Turtle
    { name :: Text
    , onBackOf :: Turtle
    } deriving (Generic, Wire.Rep)
