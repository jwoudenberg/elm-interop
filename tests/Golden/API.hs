{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Golden.API
  ( API
  ) where

import Data.Text (Text)
import Data.Void (Void)
import Elm.Servant (ELM)
import GHC.Generics (Generic)
import Servant.API
import qualified Wire

type API = "api" :> Capture "hi" Int :> ReqBody '[ ELM] Bar :> Post '[ ELM] Foo

data Foo = Foo
  { one :: Either Int (Int, Text, Text)
  , two :: ()
  , three :: Maybe [Text]
  , four :: Maybe Bar
  } deriving (Generic)

instance Wire.Rep Foo

data Bar
  = Bar Void
        Int
        Foo
  | Baz
  deriving (Generic)

instance Wire.Rep Bar

data Unicorn
  deriving (Generic)

instance Wire.Rep Unicorn
