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
  , WrappedField(WrappedField)
  , primitive
  , handle
  , tuple2
  , many
  , record
  , union
  , IsField(getField, getLabel, (=:))
  , match
  ) where

import Data.Aeson ((.:))
import Data.Aeson.Types (Pair, Parser)
import Data.Functor.Invariant (Invariant(invmap))
import Data.Maybe (fromMaybe)
import Data.Monoid (First(First, getFirst))
import Data.Proxy (Proxy(Proxy))
import Data.Vinyl
  ( ElField
  , ElField(Field)
  , Label(Label)
  , RElem
  , Rec((:&), RNil)
  , rapply
  , recordToList
  , rfoldMap
  , rget
  , rmap
  , rpureConstrained
  , rtraverse
  )
import Data.Vinyl.CoRec
  ( CoRec(CoRec)
  , FoldRec
  , Handler(H)
  , Handlers
  , coRecTraverse
  , firstField
  )
import Data.Vinyl.Functor
  ( (:.)
  , Compose(Compose, getCompose)
  , Const(Const, getConst)
  , Identity(Identity)
  , Identity
  , Lift(Lift)
  )
import Data.Vinyl.TypeLevel (RIndex)
import GHC.Exts (toList)
import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)

import qualified Data.Aeson as Aeson
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text as T

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
    toRecord (a, b) = (Label @"_1" =: a) :& (Label @"_2" =: b) :& RNil
    fromRecord :: Rec ElField '[ '( "_1", a), '( "_2", b)] -> (a, b)
    fromRecord (x :& y :& RNil) = (getField x, getField y)

{-
  Like `ElField` from `Vinyl`, except that it allows you to specify a wrapping
  functor. Not sure if there's a way to use Vinyl that doesn't require this.
-}
data WrappedField f (field :: (Symbol, k)) where
  WrappedField :: KnownSymbol s => !(f a) -> WrappedField f '( s, a)

-- |
-- We use both `ElField` and `WrappedField` in this implementation. To not have
-- to understand the difference outside of the context of this module, we set up
-- some helpers that work with either field type.
class IsField field where
  type Stored field a
  (=:) ::
       (KnownSymbol l)
    => Label (l :: Symbol)
    -> Stored field a
    -> field '( l, a)
  getField :: field '( l, a) -> Stored field a
  getLabel :: (KnownSymbol s) => field '( s, a) -> T.Text

instance IsField (WrappedField f) where
  type Stored (WrappedField f) a = f a
  _ =: x = WrappedField x
  getField (WrappedField x) = x
  getLabel ::
       forall f s a. (KnownSymbol s)
    => WrappedField f '( s, a)
    -> T.Text
  getLabel _ = T.pack $ symbolVal (Proxy :: Proxy s)

instance IsField ElField where
  type Stored ElField a = a
  _ =: x = Field x
  getField (Field x) = x
  getLabel ::
       forall s a. (KnownSymbol s)
    => ElField '( s, a)
    -> T.Text
  getLabel _ = T.pack $ symbolVal (Proxy :: Proxy s)

record :: forall xs. Rec (WrappedField Coder) xs -> Coder (Rec ElField xs)
record coders =
  Coder
    { encode = Aeson.object . recordToList . rapply (recordEncoders coders)
    , decode =
        Aeson.withObject "Record" $ \o ->
          rtraverse (\(WrappedField x) -> parseField o x) coders
    }

parseField ::
     forall s t. (KnownSymbol s)
  => Aeson.Object
  -> Coder t
  -> Parser (ElField '( s, t))
parseField object coder =
  maybe
    (fail ("Expected key not found: " <> T.unpack key))
    (fmap Field . decode coder)
    (HashMap.lookup key object)
  where
    key :: T.Text
    key = T.pack $ symbolVal (Proxy :: Proxy s)

recordEncoders ::
     Rec (WrappedField Coder) xs -> Rec (Lift (->) ElField (Const Pair)) xs
recordEncoders =
  rmap (\coder -> Lift (\(Field x) -> Const $ runEncoder coder x))

runEncoder ::
     forall s t. (KnownSymbol s)
  => WrappedField Coder '( s, t)
  -> t
  -> Pair
runEncoder coder x = (getLabel coder, encode (getField coder) x)

union ::
     forall x xs. (FoldRec (x ': xs) (x ': xs))
  => Rec (WrappedField Coder) (x ': xs)
  -> Coder (CoRec ElField (x ': xs))
union coders =
  Coder
    { encode = encode variant . flip match (recordEncoders coders)
    , decode = (=<<) (decodeVariant coders) . decode variant
    }
  where
    variant = tuple2 primitive primitive

decodeVariant ::
     forall xs. (FoldRec xs xs)
  => Rec (WrappedField Coder) xs
  -> Pair
  -> Parser (CoRec ElField xs)
decodeVariant coders pair@(key, _) =
  maybe (fail errorMsg) (coRecTraverse getCompose) . firstField $
  chooseCoder pair coders
  where
    errorMsg = "Unexpected variant for sum: " <> T.unpack key

chooseCoder ::
     Pair -> Rec (WrappedField Coder) xs -> Rec (Maybe :. Parser :. ElField) xs
chooseCoder pair = rmap (\t@(WrappedField x) -> runDecoder pair t)

runDecoder ::
     forall s t.
     Pair
  -> WrappedField Coder '( s, t)
  -> (Maybe :. Parser :. ElField) '( s, t)
runDecoder (key, value) t@(WrappedField coder) =
  Compose $
  if label == key
    then Just . Compose . fmap Field $decode coder value
    else Nothing
  where
    label = getLabel t

-- |
-- Custom version of `match`, based on the one in `Data.Vinyl.CoRec`.
-- The library version of `match` only works with a `CoRec Identity`, and so
-- couldn't be used here.
--
-- Extra bonus of writing our own match is that we can write an API more
-- similar to `rapply`, allowing us to reuse the `recordEncoders` logic for
-- records and unions both.
match :: CoRec ElField xs -> Rec (Lift (->) ElField (Const b)) xs -> b
match (CoRec x) hs =
  case rget Proxy hs of
    Lift f -> getConst (f x)

handle :: (a -> b) -> Lift (->) ElField (Const b) '( s, a)
handle f = Lift (\(Field x) -> Const (f x))
