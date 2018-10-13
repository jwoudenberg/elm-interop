{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}

module Coder.Rec
  ( rec
  ) where

import Coder.Internal (Coder(Coder, decode, encode))
import Data.Aeson.Types (Parser)
import Data.Vinyl (Rec((:&), RNil), rapply, recordToList, rmap)
import Data.Vinyl.Functor (Const(Const), Identity(Identity), Lift(Lift))
import GHC.Exts (toList)

import qualified Data.Aeson as Aeson

rec :: forall xs. Rec Coder xs -> Coder (Rec Identity xs)
rec coders =
  Coder
    { encode = Aeson.toJSON . recordToList . rapply (encoders coders)
    , decode = Aeson.withArray "Parameter List" (decoders coders . toList)
    }

decoders :: Rec Coder xs -> [Aeson.Value] -> Parser (Rec Identity xs)
decoders RNil [] = pure RNil
decoders RNil _ = fail "Decoded parameter list is longer than expected."
decoders (c :& cs) (x:xs) =
  (:&) <$> (Identity <$> decode c x) <*> decoders cs xs
decoders _ [] = fail "Decoded parameter list is shorter than expected."

encoders :: Rec Coder xs -> Rec (Lift (->) Identity (Const Aeson.Value)) xs
encoders = rmap (\coder -> Lift (\(Identity x) -> Const $ encode coder x))
