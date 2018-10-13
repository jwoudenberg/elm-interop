module Coder.Internal
  ( Coder(Coder, encode, decode)
  , primitive
  ) where

import Data.Aeson.Types (Parser)
import Data.Functor.Invariant (Invariant(invmap))

import qualified Data.Aeson as Aeson

data Coder a = Coder
  { encode :: a -> Aeson.Value
  , decode :: Aeson.Value -> Parser a
  }

instance Invariant Coder where
  invmap to from (Coder enc dec) = Coder (enc . from) (fmap to . dec)

{-
  We seek to ensure encoding and decoding for Haskell end Elm are compatible.
  To this end we want to write our own encoding and decoding logic, or else
  we'd be dependent on the implementation of an external library (Aeson) for
  the functioning of our own code: we'd need to ensure our Elm logic encodes
  and decodes the exact same way Aeson does internally.

  We make an exception for primitives. We do not seek to replicate the logic
  Aeson has for ensuring integers and correctly parsed from JSON numbers, and
  the like.
-}
primitive :: (Aeson.ToJSON a, Aeson.FromJSON a) => Coder a
primitive = Coder {encode = Aeson.toJSON, decode = Aeson.parseJSON}
