{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module Examples.QueryParams
  ( API,
  )
where

import qualified Data.Text as T
import GHC.Generics (Generic)
import Servant.API
import Servant.Interop (ParameterType, Rep, WIRE)

type API =
  "groceries"
    :> QueryParam "max-price" EuroCents
    :> QueryFlag "bio"
    :> QueryParams "brands" T.Text
    :> Get '[WIRE] [Grocery]

newtype EuroCents = EuroCents Int
  deriving (Generic)

instance ParameterType EuroCents

data Grocery
  = Courgette
  | Milk
  | PeanutButter
  | Chocolate
  deriving (Generic)

instance Rep Grocery
