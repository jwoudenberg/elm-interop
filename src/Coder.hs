{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ConstraintKinds #-}

module Coder
  ( Coder(encode, decode)
  , primitive
  , tuple2
  , many
  , record
  , union
  ) where

import Data.Aeson ((.:))
import Data.Aeson.Types (Pair, Parser)
import Data.Functor.Invariant (Invariant(invmap))
import Data.Maybe (fromMaybe)
import Data.Monoid (First(First, getFirst))
import Data.Proxy (Proxy(Proxy))
import Data.Vinyl
  ( Label(Label)
  , RElem
  , Rec((:&), RNil)
  , rapply
  , recordToList
  , rfoldMap
  , rget
  , rmap
  , rtraverse
  )
import Data.Vinyl.CoRec (CoRec(CoRec), FoldRec, coRecTraverse, firstField)
import Data.Vinyl.Functor
  ( (:.)
  , Compose(Compose, getCompose)
  , Const(Const, getConst)
  , Identity(Identity, getIdentity)
  , Identity
  , Lift(Lift)
  )
import Data.Vinyl.POP (POP)
import Data.Vinyl.Record (Field(Field), Record, (=:), getField, getLabel)
import Data.Vinyl.SOP (SOP)
import Data.Vinyl.Sum (Op(Op), Sum)
import Data.Vinyl.TypeLevel (RIndex)
import GHC.Exts (toList)
import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)

import qualified Data.Aeson as Aeson
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text as T
import qualified Data.Vinyl as Vinyl
import qualified Data.Vinyl.POP
import qualified Data.Vinyl.Record as Record
import qualified Data.Vinyl.SOP
import qualified Data.Vinyl.Sum as Sum

data Coder a = Coder
  { encode :: a -> Aeson.Value
  , decode :: Aeson.Value -> Parser a
  }

type Decoder a = Aeson.Value -> Parser a

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

many :: (Foldable f, Applicative f, Monoid (f a)) => Coder a -> Coder (f a)
many elem =
  Coder
    { encode = Aeson.Array . foldMap (pure . encode elem)
    , decode =
        Aeson.withArray "[a]" (fmap (foldMap pure) . traverse (decode elem))
    }

tuple2 :: Coder a -> Coder b -> Coder (a, b)
tuple2 first second =
  invmap fromRecord toRecord . record $
  (Label @"_1" =: first) :& (Label @"_2" =: second) :& RNil
  where
    toRecord (a, b) = (Label @"_1" =: pure a) :& (Label @"_2" =: pure b) :& RNil
    fromRecord :: Record Identity '[ '( "_1", a), '( "_2", b)] -> (a, b)
    fromRecord (x :& y :& RNil) =
      (getIdentity $ getField x, getIdentity $ getField y)

record :: forall xs. Record Coder xs -> Coder (Record Identity xs)
record coders =
  Coder
    { encode = Aeson.object . recordToList . rapply (recordEncoders coders)
    , decode =
        Aeson.withObject "Record" $ \o ->
          rtraverse (\(Field x) -> parseField o x) coders
    }

recordEncoders ::
     Record Coder xs -> Rec (Lift (->) (Field Identity) (Const Pair)) xs
recordEncoders =
  rmap (\coder -> Lift (\(Field x) -> Const $ runEncoder coder (getIdentity x)))

parseField ::
     forall s t. (KnownSymbol s)
  => Aeson.Object
  -> Coder t
  -> Parser (Field Identity '( s, t))
parseField object coder =
  maybe
    (fail ("Expected key not found: " <> T.unpack key))
    (fmap (Field . pure) . decode coder)
    (HashMap.lookup key object)
  where
    key :: T.Text
    key = T.pack $ symbolVal (Proxy :: Proxy s)

runEncoder ::
     forall s t f. (KnownSymbol s)
  => Field Coder '( s, t)
  -> t
  -> Pair
runEncoder coder x = (getLabel coder, encode (getField coder) x)

union ::
     forall x xs f. (FoldRec (x ': xs) (x ': xs))
  => POP Coder (x ': xs)
  -> Coder (SOP Identity (x ': xs))
union coders =
  Coder
    { encode = encode variant . flip Sum.match (unionEncoders coders)
    , decode = (=<<) (decodeVariant coders) . decode variant
    }
  where
    variant = tuple2 primitive primitive

unionEncoders :: POP Coder ys -> Rec (Op Pair :. Field (Rec Identity)) ys
unionEncoders =
  rmap (Compose . (\coder -> Op (\(Field x) -> encodeRec coder x)))

encodeRec ::
     forall s ts. (KnownSymbol s)
  => Field (Rec Coder) '( s, ts)
  -> Rec Identity ts
  -> Pair
encodeRec t@(Field coders) x = (getLabel t, encode (rec coders) x)

rec :: forall xs. Rec Coder xs -> Coder (Rec Identity xs)
rec coders =
  Coder
    { encode = Aeson.toJSON . recordToList . rapply (recEncoders coders)
    , decode = Aeson.withArray "Parameter List" (decodeArray coders . toList)
    }

decodeArray :: Rec Coder xs -> [Aeson.Value] -> Parser (Rec Identity xs)
decodeArray RNil [] = pure RNil
decodeArray RNil _ = fail "Decoded parameter list is longer than expected."
decodeArray (c :& cs) (x:xs) =
  (:&) <$> (Identity <$> decode c x) <*> decodeArray cs xs
decodeArray _ [] = fail "Decoded parameter list is shorter than expected."

recEncoders :: Rec Coder xs -> Rec (Lift (->) Identity (Const Aeson.Value)) xs
recEncoders = rmap (\coder -> Lift (\(Identity x) -> Const $ encode coder x))

decodeVariant ::
     forall xs. (FoldRec xs xs)
  => POP Coder xs
  -> Pair
  -> Parser (SOP Identity xs)
decodeVariant coders pair@(key, _) =
  maybe (fail errorMsg) (coRecTraverse getCompose) . firstField $
  chooseCoder pair coders
  where
    errorMsg = "Unexpected variant for sum: " <> T.unpack key

chooseCoder ::
     Pair -> POP Coder xs -> Rec (Maybe :. Parser :. Field (Rec Identity)) xs
chooseCoder pair = rmap (\t@(Field x) -> runDecoder pair t)

runDecoder ::
     forall s t f.
     Pair
  -> Field (Rec Coder) '( s, t)
  -> (Maybe :. Parser :. Field (Rec Identity)) '( s, t)
runDecoder (key, value) t@(Field coders) =
  Compose $
  if label == key
    then Just . Compose . fmap Field $decode (rec coders) value
    else Nothing
  where
    label = getLabel t
