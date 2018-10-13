{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Coder.Record
  ( record
  ) where

import Coder.Internal (Coder(Coder, decode, encode))
import Data.Aeson.Types (Pair, Parser)
import Data.Proxy (Proxy(Proxy))
import Data.Vinyl (Rec, rapply, recordToList, rmap, rtraverse)
import Data.Vinyl.Functor
  ( Const(Const)
  , Identity(getIdentity)
  , Identity
  , Lift(Lift)
  )
import Data.Vinyl.Record (Field(Field), Record, getField, getLabel)
import GHC.TypeLits (KnownSymbol, symbolVal)

import qualified Data.Aeson as Aeson
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text as T

record :: forall xs. Record Coder xs -> Coder (Record Identity xs)
record coders =
  Coder
    { encode = Aeson.object . recordToList . rapply (encoders coders)
    , decode =
        Aeson.withObject "Record" $ \o ->
          rtraverse (\(Field x) -> fieldParser o x) coders
    }

encoders :: Record Coder xs -> Rec (Lift (->) (Field Identity) (Const Pair)) xs
encoders =
  rmap (\coder -> Lift (\(Field x) -> Const $ runEncoder coder (getIdentity x)))

fieldParser ::
     forall s t. (KnownSymbol s)
  => Aeson.Object
  -> Coder t
  -> Parser (Field Identity '( s, t))
fieldParser object coder =
  maybe
    (fail ("Expected key not found: " <> T.unpack key))
    (fmap (Field . pure) . decode coder)
    (HashMap.lookup key object)
  where
    key :: T.Text
    key = T.pack $ symbolVal (Proxy :: Proxy s)

runEncoder ::
     forall s t. (KnownSymbol s)
  => Field Coder '( s, t)
  -> t
  -> Pair
runEncoder coder x = (getLabel coder, encode (getField coder) x)
