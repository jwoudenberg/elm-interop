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
import Servant.Interop (ParameterType, Rep, WIRE)

type API =
  ListDogs :<|> GetToy :<|> PostToy

type ListDogs =
  "dogs"
    :> Get '[WIRE] [Dog]

type GetToy =
  "toys"
    :> Get '[WIRE] Toy

type PostToy =
  "toys"
    :> Header "auth-smell" SmellRight
    :> ReqBody '[WIRE] Toy
    :> Post '[WIRE] ()

newtype SmellRight = SmellRight Bool
  deriving (Generic)

instance ParameterType SmellRight

data Dog
  = Dog
      { name :: Name,
        age :: Int
      }
  deriving (Generic)

newtype Name = Name Text deriving (Generic)

instance Rep Name

instance Rep Dog

data Toy
  = Bone
  | Ball
  deriving (Generic)

instance Rep Toy
