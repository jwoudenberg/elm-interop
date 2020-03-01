{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module Examples.MultipleEndpoints
  ( API,
  )
where

import Data.Text (Text)
import GHC.Generics (Generic)
import Servant.API
import Servant.Interop (Rep, WIRE)

type API =
  ListDogs :<|> GetToy

type ListDogs =
  "dogs"
    :> Get '[WIRE] [Dog]

type GetToy =
  "toys"
    :> Get '[WIRE] Toy

newtype SmellRight = SmellRight Bool
  deriving (Generic)

data Dog
  = Dog
      { name :: Name,
        age :: Int
      }
  deriving (Generic)

instance Rep Dog

newtype Name = Name Text
  deriving (Generic)

instance Rep Name

data Toy
  = Bone
  | Ball
  deriving (Generic)

instance Rep Toy
