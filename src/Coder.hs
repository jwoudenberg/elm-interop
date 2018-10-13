module Coder
  ( Coder(encode, decode)
  , primitive
  , tuple2
  , many
  , record
  , customType
  ) where

import Coder.CustomType (customType)
import Coder.Internal (Coder(Coder, decode, encode), primitive)
import Coder.Record (record)
import Coder.Tuple2 (tuple2)

import qualified Data.Aeson as Aeson

many :: (Foldable f, Applicative f, Monoid (f a)) => Coder a -> Coder (f a)
many x =
  Coder
    { encode = Aeson.Array . foldMap (pure . encode x)
    , decode = Aeson.withArray "[a]" (fmap (foldMap pure) . traverse (decode x))
    }
