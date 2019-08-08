{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Data.Foldable (toList)
import Data.Functor.Foldable (Fix(Fix), para, unfix, zygo)
import Data.HashMap.Strict.InsOrd (InsOrdHashMap)
import Data.List.NonEmpty (NonEmpty)
import Data.Proxy (Proxy(Proxy))
import Data.Text (Text)
import Data.Void (Void)
import GHC.Generics
import GHC.TypeLits (KnownSymbol, symbolVal)
import Text.PrettyPrint.Leijen.Text ((<+>))

import qualified Data.HashMap.Strict.InsOrd as HashMap
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Text as Text
import qualified Text.PrettyPrint.Leijen.Text as PP

-- |
-- Small test of functionality in this library. Will be removed before release.
main :: IO ()
main =
  putStrLn .
  Text.unpack . showDoc . PP.vcat . fmap printTypeDefinition . typeDefinitions $
  hasElmType (Proxy :: Proxy Foo)

data Foo = Foo
  { one :: ()
  , two :: Int
  , three :: Text
  , four :: Bar
  } deriving (Generic)

instance HasElmType Foo

data Bar
  = Bar Int
        Text
  | Baz
  deriving (Generic)

instance HasElmType Bar

data ElmTypeF a
  = Unit
  | Never
  | Bool
  | Int
  | Float
  | String
  | List a
  | Maybe a
  | Tuple2 a
           a
  | Tuple3 a
           a
           a
  | Record (InsOrdHashMap Text a)
  | Lambda a
           a
  | Defined (ElmTypeDefinitionF a)
  deriving (Functor, Show)

type ElmType = Fix ElmTypeF

data ElmTypeDefinitionF a
  = Custom Text
           (NonEmpty (Text, [a]))
  | Alias Text
          a
  deriving (Functor, Show)

type ElmTypeDefinition = ElmTypeDefinitionF ElmType

showDoc :: PP.Doc -> Text
showDoc = PP.displayTStrict . PP.renderPretty 1 40

printType :: ElmType -> PP.Doc
printType =
  zygo appearance $ \case
    Unit -> "()"
    Never -> "Never"
    Bool -> "Bool"
    Int -> "Int"
    Float -> "Float"
    String -> "String"
    List (a, i) -> "List" <+> parens a i
    Maybe (a, i) -> "Maybe" <+> parens a i
    Tuple2 (_, i) (_, j) -> encloseSep' PP.lparen PP.rparen PP.comma [i, j]
    Tuple3 (_, i) (_, j) (_, k) ->
      encloseSep' PP.lparen PP.rparen PP.comma [i, j, k]
    Record xs -> encloseSep' PP.lbrace PP.rbrace PP.comma fields
      where fields = printRecordField <$> HashMap.toList xs
    Defined (Custom n _) -> PP.textStrict n
    Defined (Alias n _) -> PP.textStrict n
    Lambda (ai, i) (_, o) -> idoc <+> "->" <+> o
      -- |
      -- We only need to parenthesize the input argument if it is a
      -- lambda function itself.
      where idoc =
              case ai of
                SingleWord -> i
                MultipleWord -> i
                MultipleWordLambda -> PP.parens i

printTypeDefinition :: ElmTypeDefinition -> PP.Doc
printTypeDefinition =
  \case
    Custom name constructors ->
      "type" <+> PP.textStrict name <> PP.linebreak <> printedConstructors
      where printedConstructors =
              PP.indent elmIndent . PP.vcat . zipWith (<+>) ("=" : repeat "|") $
              printConstructor <$> toList constructors
    Alias name base ->
      "type alias" <+>
      PP.textStrict name <+>
      "=" <> PP.line <> PP.indent elmIndent (printType base)

printConstructor :: (Text, [ElmType]) -> PP.Doc
printConstructor (name, params) =
  PP.nest elmIndent (PP.sep (PP.textStrict name : (printParam <$> params)))
  where
    printParam :: ElmType -> PP.Doc
    printParam t = parens (appearance (unfix t)) (printType t)

elmIndent :: Int
elmIndent = 4

typeDefinitions :: ElmType -> [ElmTypeDefinition]
typeDefinitions =
  para $ \case
    Unit -> mempty
    Never -> mempty
    Bool -> mempty
    Int -> mempty
    Float -> mempty
    String -> mempty
    List (_, x) -> x
    Maybe (_, x) -> x
    Tuple2 (_, x) (_, y) -> x <> y
    Tuple3 (_, x) (_, y) (_, z) -> x <> y <> z
    Record x -> mconcat $ snd <$> HashMap.elems x
    Defined (Alias n (t, x)) -> Alias n t : x
    Defined (Custom n x) ->
      Custom n (fmap (fmap fst) <$> x) :
      (mconcat . fmap snd . mconcat . fmap snd $ toList x)
    Lambda (_, x) (_, y) -> x <> y

-- |
-- Version of `encloseSep` that puts the closing delimiter on a new line, and
-- adds a space between the separator and the content.
--
-- Used for printing lists and records in a fashion compatible with elm-format.
encloseSep' :: PP.Doc -> PP.Doc -> PP.Doc -> [PP.Doc] -> PP.Doc
encloseSep' left right sp ds =
  case ds of
    [] -> left <> right
    [d] -> left <+> d <+> right
    _ -> PP.group $ PP.vcat entries <> PP.line <> right
      where entries = zipWith (<+>) (left : repeat sp) ds

data TypeAppearance
  = SingleWord
  -- ^ The printed type consists of a single word, like `Int` or `Thing`.
  | MultipleWord
  -- ^ The printed type consists of multiple words, like `List Int`
  | MultipleWordLambda
  -- ^ The type is a lambda, like `Text -> Int`. Implies `MultipleWord`.

appearance :: ElmTypeF a -> TypeAppearance
appearance =
  \case
    Unit -> SingleWord
    Never -> SingleWord
    Bool -> SingleWord
    Int -> SingleWord
    Float -> SingleWord
    String -> SingleWord
    List _ -> MultipleWord
    Maybe _ -> MultipleWord
    Tuple2 _ _ -> SingleWord
    Tuple3 _ _ _ -> SingleWord
    Record _ -> SingleWord
    Defined _ -> SingleWord
    Lambda _ _ -> MultipleWordLambda

parens :: TypeAppearance -> PP.Doc -> PP.Doc
parens a doc =
  case a of
    SingleWord -> doc
    MultipleWord -> PP.parens doc
    MultipleWordLambda -> PP.parens doc

printRecordField :: (Text, (a, PP.Doc)) -> PP.Doc
printRecordField (k, (_, v)) = PP.textStrict k <+> ":" <+> v

-- |
-- Class for Haskell types that have an Elm type. The goal is for this class
-- to be able to generate Elm representations for all Haskell types.
class HasElmType (a :: *) where
  hasElmType :: Proxy a -> ElmType
  default hasElmType :: (HasElmTypeG (Rep a)) =>
    Proxy a -> ElmType
  hasElmType _ = hasElmTypeG (Proxy :: Proxy (Rep a))

instance HasElmType () where
  hasElmType _ = Fix Unit

instance HasElmType Void where
  hasElmType _ = Fix Never

instance HasElmType Int where
  hasElmType _ = Fix Int

instance HasElmType Text where
  hasElmType _ = Fix String

instance HasElmType a => HasElmType [a] where
  hasElmType _ = Fix $ List (hasElmType (Proxy :: Proxy a))

-- |
-- Helper class for constructing elm types from generics primitives. The
-- GHC.Generics recommends you create such a class, to allow you to work on
-- kinds * -> * instead of kinds * that the `HasElmType` class works on.
class HasElmTypeG (f :: * -> *) where
  hasElmTypeG :: Proxy f -> ElmType

instance HasElmTypeG V1 where
  hasElmTypeG _ = Fix Never

instance HasElmTypeG U1 where
  hasElmTypeG _ = Fix Unit

instance HasElmType c => HasElmTypeG (K1 i c) where
  hasElmTypeG _ = hasElmType (Proxy :: Proxy c)

instance (HasElmSumG f, HasDataName c) => HasElmTypeG (M1 D c f) where
  hasElmTypeG _ =
    mkElmSum (hasDataName (Proxy :: Proxy c)) (hasElmSumG (Proxy :: Proxy f))

-- |
-- Helper class for constructing sum types.
class HasElmSumG (f :: * -> *) where
  hasElmSumG :: Proxy f -> ElmSum

newtype ElmSum =
  ElmSum [(Text, ElmProduct)]
  deriving (Semigroup, Monoid)

instance HasElmSumG V1 where
  hasElmSumG _ = mempty

instance (HasElmProductG f, HasConstructorName c) => HasElmSumG (M1 C c f) where
  hasElmSumG _ =
    ElmSum
      [ ( hasConstructorName (Proxy :: Proxy c)
        , hasElmProductG (Proxy :: Proxy f))
      ]

instance (HasElmSumG f, HasElmSumG g) => HasElmSumG (f :+: g) where
  hasElmSumG _ = hasElmSumG (Proxy :: Proxy f) <> hasElmSumG (Proxy :: Proxy g)

mkElmSum :: Text -> ElmSum -> ElmType
mkElmSum name (ElmSum sum') =
  case NonEmpty.nonEmpty sum' of
    Nothing -> Fix Never
    Just nonEmptySum ->
      Fix . Defined . Custom name . (fmap . fmap) mkElmProduct $ nonEmptySum

-- |
-- Helper class for constructing product types. We don't decide yet the type of
-- product we're constructing (tuple, record, parameter list), so we can reuse
-- this logic for all those products.
class HasElmProductG (f :: * -> *) where
  hasElmProductG :: Proxy f -> ElmProduct
  default hasElmProductG :: HasElmTypeG f =>
    Proxy f -> ElmProduct
  hasElmProductG x = ElmProduct [(Nothing, hasElmTypeG x)]

newtype ElmProduct =
  ElmProduct [(Maybe Text, ElmType)]
  deriving (Semigroup)

instance HasElmType c => HasElmProductG (K1 i c)

instance HasElmProductG V1

instance HasElmProductG U1

instance (HasElmTypeG f, HasFieldName c) => HasElmProductG (M1 S c f) where
  hasElmProductG _ =
    ElmProduct
      [(hasFieldName (Proxy :: Proxy c), hasElmTypeG (Proxy :: Proxy f))]

instance (HasElmProductG f, HasElmProductG g) => HasElmProductG (f :*: g) where
  hasElmProductG _ =
    hasElmProductG (Proxy :: Proxy f) <> hasElmProductG (Proxy :: Proxy g)

mkElmProduct :: ElmProduct -> [ElmType]
mkElmProduct (ElmProduct prod) =
  case traverse fst prod of
    Just names -> [Fix . Record . HashMap.fromList $ zip names values]
    Nothing -> values
  where
    values = snd <$> prod

mkElmTuple :: [ElmType] -> ElmType
mkElmTuple values =
  case values of
    [] -> Fix Unit
    [x] -> x
    [x, y] -> Fix $ Tuple2 x y
    [x, y, z] -> Fix $ Tuple3 x y z
    -- Elm only has tuples with 2 or 3 elements. If we have more values
    -- than that we have to use a record.
    _ -> Fix . Record . HashMap.fromList $ zip anonFields (toList values)
      where anonFields = ("field" <>) . Text.pack . show <$> ([1 ..] :: [Int])

-- |
-- Helper class for extracting a name from a generics 'MetaSel type.
class HasFieldName (f :: Meta) where
  hasFieldName :: Proxy f -> Maybe Text

instance HasFieldName ('MetaSel 'Nothing su ss ds) where
  hasFieldName _ = Nothing

instance KnownSymbol n => HasFieldName ('MetaSel ('Just n) su ss ds) where
  hasFieldName _ = Just . Text.pack $ symbolVal (Proxy :: Proxy n)

-- |
-- Helper class for extracting a name from a generics 'MetaCons type.
class HasConstructorName (a :: Meta) where
  hasConstructorName :: Proxy a -> Text

instance KnownSymbol n => HasConstructorName ('MetaCons n f s) where
  hasConstructorName _ = Text.pack $ symbolVal (Proxy :: Proxy n)

-- |
-- Helper class for extracting a name from a generics 'MetaData type.
class HasDataName (a :: Meta) where
  hasDataName :: Proxy a -> Text

instance KnownSymbol n => HasDataName ('MetaData n m p nt) where
  hasDataName _ = Text.pack $ symbolVal (Proxy :: Proxy n)
