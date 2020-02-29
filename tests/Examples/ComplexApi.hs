{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module Examples.ComplexApi
  ( API,
  )
where

import Data.Text (Text)
import GHC.Generics (Generic)
import Servant.API
import Servant.Interop (ParameterType, Rep, WIRE)

type API =
  GetDog :<|> ListDogs :<|> GetToy :<|> PostToy

type GetDog =
  "dogs"
    :> Capture "name" Name
    :> Get '[WIRE] Dog

type ListDogs =
  "dogs"
    :> QueryParam "min-age" Int
    :> Get '[WIRE] Dog

type GetToy =
  "toys"
    :> QueryFlag "fun"
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

newtype Name = Name Text deriving (Generic, Rep)

instance ParameterType Name

instance Rep Dog

data Toy
  = Bone
  | Ball
  deriving (Generic)

instance Rep Toy
