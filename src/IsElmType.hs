{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module IsElmType
  ( IsElmType(..)
  , typeAST
  , coder
  ) where

import Data.Functor.Foldable (Fix(Fix))
import Data.Functor.Invariant (Invariant(invmap))
import Data.HashMap.Strict (HashMap)
import Data.Vinyl (Label(Label), Rec((:&), RNil), rfoldMap, rmap)
import Data.Vinyl.CoRec (CoRec(CoRec), FoldRec)
import Data.Vinyl.Functor
  ( (:.)
  , Compose(Compose)
  , Identity(Identity, getIdentity)
  )
import Data.Vinyl.POP (POP)
import Data.Vinyl.Record
  ( Field(Field)
  , Record
  , (=:)
  , getField
  , getLabel
  , labelVal
  )
import Data.Vinyl.SOP (SOP)
import GHC.TypeLits (KnownSymbol)

import qualified Coder
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text as T
import qualified Data.Vinyl.Record as Record
import qualified Data.Vinyl.Sum as Sum
import qualified ElmType

-- |
-- Haskell equivalents of Elm's basic types.
-- We implement serialization and Elm code generation for these basic types.
-- Interop to some other Haskell type X can be implemented by defining an
-- isomorphism between X and an `IsElmType a`.
data IsElmType a where
  ElmInt :: IsElmType Int
  ElmString :: IsElmType String
  ElmList :: IsElmType a -> IsElmType [a]
  ElmTuple2 :: IsElmType a -> IsElmType b -> IsElmType (a, b)
  ElmResult :: IsElmType a -> IsElmType b -> IsElmType (Either a b)
  ElmRecord :: Record IsElmType xs -> IsElmType (Record Identity xs)
  ElmCustomType
    :: (KnownSymbol s, FoldRec (x ': xs) (x ': xs))
    => Label s
    -> POP IsElmType (x ': xs)
    -> IsElmType (Label s, SOP Identity (x ': xs))

typeAST :: IsElmType a -> ElmType.ElmType
typeAST =
  Fix . \case
    ElmInt -> ElmType.ElmInt
    ElmString -> ElmType.ElmString
    ElmList x -> ElmType.ElmList (typeAST x)
    ElmTuple2 x y -> ElmType.ElmTuple2 (typeAST x) (typeAST y)
    ElmResult l r ->
      ElmType.ElmResult {ElmType.ok = typeAST r, ElmType.err = typeAST l}
    ElmRecord rec -> ElmType.ElmRecord . rfoldMap fieldToAST $ rec
    ElmCustomType l ctors ->
      ElmType.ElmCustomType (labelVal l) . rfoldMap ctorAST $ ctors

fieldToAST :: Field IsElmType a -> HashMap T.Text ElmType.ElmType
fieldToAST t@(Field elmType) = HashMap.singleton (getLabel t) (typeAST elmType)

ctorAST :: Field (Rec IsElmType) a -> HashMap T.Text [ElmType.ElmType]
ctorAST t@(Field toElm) =
  HashMap.singleton (getLabel t) (rfoldMap (pure . typeAST) toElm)

coder :: IsElmType a -> Coder.Coder a
coder =
  \case
    ElmInt -> Coder.primitive
    ElmString -> Coder.primitive
    ElmList x -> Coder.many (coder x)
    ElmTuple2 x y -> Coder.tuple2 (coder x) (coder y)
    ElmResult left right ->
      invmap toEither fromEither . Coder.customType $
      Label @"_left" =: (coder left :& RNil) :& Label @"_right" =:
      (coder right :& RNil) :&
      RNil
    ElmRecord rec ->
      Coder.record (rmap (\(Field elmType) -> Field (coder elmType)) rec)
    ElmCustomType l ctors ->
      invmap (l, ) snd . Coder.customType . Record.rmap (rmap coder) $ ctors

type EitherSOP f l r = SOP f '[ '( "_left", '[ l]), '( "_right", '[ r])]

toEither :: EitherSOP Identity l r -> Either l r
toEither coRec = Sum.match coRec $ handle Left :& handle Right :& RNil

fromEither :: forall l r. Either l r -> EitherSOP Identity l r
fromEither (Left l) =
  CoRec (Field (Identity l :& RNil) :: Field (Rec Identity) '( "_left", '[ l]))
fromEither (Right r) =
  CoRec (Field (Identity r :& RNil) :: Field (Rec Identity) '( "_right", '[ r]))

handle :: (b -> a) -> (Sum.Op a :. Field (Rec Identity)) '( s, '[ b])
handle f = Compose (Sum.Op (f . getIdentity . extract . getField))

extract :: Rec f '[ x] -> f x
extract (x :& RNil) = x
