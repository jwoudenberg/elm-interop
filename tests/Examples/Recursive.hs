{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module Examples.Recursive
  ( API,
  )
where

import Data.Text (Text)
import GHC.Generics (Generic)
import Servant.API
import Servant.Interop (Rep, WIRE)

type API = "turtles" :> Get '[WIRE] Turtle

data Turtle
  = Turtle
      { name :: Text,
        onBackOf :: Turtle
      }
  deriving (Generic, Rep)
