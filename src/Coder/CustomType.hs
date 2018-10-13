{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}

module Coder.CustomType
  ( customType
  ) where

import Coder.Internal (Coder(Coder, decode, encode), primitive)
import Coder.Rec (rec)
import Coder.Tuple2 (tuple2)
import Data.Aeson.Types (Pair, Parser)
import Data.Vinyl (Rec, rmap)
import Data.Vinyl.CoRec (FoldRec, coRecTraverse, firstField)
import Data.Vinyl.Functor ((:.), Compose(Compose, getCompose), Identity)
import Data.Vinyl.POP (POP)
import Data.Vinyl.Record (Field(Field), getLabel)
import Data.Vinyl.SOP (SOP)
import Data.Vinyl.Sum (Op(Op))
import GHC.TypeLits (KnownSymbol)

import qualified Data.Text as T
import qualified Data.Vinyl.Sum as Sum

customType ::
     forall x xs. (FoldRec (x ': xs) (x ': xs))
  => POP Coder (x ': xs)
  -> Coder (SOP Identity (x ': xs))
customType coders =
  Coder
    { encode = encode variant . flip Sum.match (encoders coders)
    , decode = (=<<) (parser coders) . decode variant
    }
  where
    variant = tuple2 primitive primitive

encoders :: POP Coder ys -> Rec (Op Pair :. Field (Rec Identity)) ys
encoders = rmap (Compose . (\coder -> Op (\(Field x) -> encodeRec coder x)))

parser ::
     forall xs. (FoldRec xs xs)
  => POP Coder xs
  -> Pair
  -> Parser (SOP Identity xs)
parser coders pair@(key, _) =
  maybe (fail errorMsg) (coRecTraverse getCompose) . firstField $
  chooseConstructor pair coders
  where
    errorMsg = "Unexpected variant for sum: " <> T.unpack key

encodeRec ::
     forall s ts. (KnownSymbol s)
  => Field (Rec Coder) '( s, ts)
  -> Rec Identity ts
  -> Pair
encodeRec t@(Field coders) x = (getLabel t, encode (rec coders) x)

chooseConstructor ::
     Pair -> POP Coder xs -> Rec (Maybe :. Parser :. Field (Rec Identity)) xs
chooseConstructor pair = rmap (\t@(Field _) -> decodeConstructor pair t)

decodeConstructor ::
     forall s t.
     Pair
  -> Field (Rec Coder) '( s, t)
  -> (Maybe :. Parser :. Field (Rec Identity)) '( s, t)
decodeConstructor (key, value) t@(Field coders) =
  Compose $
  if label == key
    then Just . Compose . fmap Field $decode (rec coders) value
    else Nothing
  where
    label = getLabel t
