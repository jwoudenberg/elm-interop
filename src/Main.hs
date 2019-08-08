{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Data.Bifunctor (first)
import Data.Foldable (toList)
import Data.Functor.Foldable (Fix(Fix), cata, para, unfix, zygo)
import Data.Int (Int32)
import Data.List.NonEmpty (NonEmpty)
import Data.Proxy (Proxy(Proxy))
import Data.Text (Text)
import Data.Void (Void, absurd)
import GHC.Generics
import GHC.TypeLits (KnownSymbol, symbolVal)
import Text.PrettyPrint.Leijen.Text ((<+>))

import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Text as Text
import qualified Text.PrettyPrint.Leijen.Text as PP

-- |
-- Small test of functionality in this library. Will be removed before release.
main :: IO ()
main = do
  putStrLn .
    Text.unpack . showDoc . PP.vcat . fmap printTypeDefinition . typeDefinitions $
    elmType (Proxy @Foo)
  putStrLn . (show :: Maybe Foo -> String) . fromElmValue . toElmValue $
    Foo {one = (), two = 42, three = "Hi!", four = Bar 12 "Bye"}

data Foo = Foo
  { one :: ()
  , two :: Int32
  , three :: Text
  , four :: Bar
  } deriving (Generic, Show)

instance HasElmType Foo

data Bar
  = Bar Int32
        Text
  | Baz
  deriving (Generic, Show)

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
  | Record [(Text, a)]
  | Lambda a
           a
  | Defined (ElmTypeDefinitionF a)
  deriving (Functor)

type ElmType = Fix ElmTypeF

data ElmTypeDefinitionF a
  = Custom Text
           (NonEmpty (Text, [a]))
  | Alias Text
          a
  deriving (Functor)

type ElmTypeDefinition = ElmTypeDefinitionF ElmType

data ElmValueF a
  = MkUnit
  | MkBool Bool
  | MkInt Int32
  | MkFloat Double
  | MkString Text
  | MkList [a]
  | MKMaybe (Maybe a)
  | MkTuple2 a
             a
  | MkTuple3 a
             a
             a
  | MkRecord [(Text, a)]
  | MkCustom Text
             [a]
  deriving (Functor)

type ElmValue = Fix ElmValueF

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
    Record xs ->
      encloseSep' PP.lbrace PP.rbrace PP.comma (printRecordField <$> xs)
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

printValue :: ElmValue -> PP.Doc
printValue =
  cata $ \case
    MkUnit -> "()"
    MkBool bool -> PP.textStrict . Text.pack $ show bool
    MkInt int -> PP.textStrict . Text.pack $ show int
    MkFloat double -> PP.textStrict . Text.pack $ show double
    MkString text -> PP.textStrict text
    MkList items -> encloseSep' PP.lbracket PP.rbracket PP.comma items
    MKMaybe a -> maybe "Nothing" ("Maybe" <+>) a
    MkTuple2 x y -> encloseSep' PP.lparen PP.rparen PP.comma [x, y]
    MkTuple3 x y z -> encloseSep' PP.lparen PP.rparen PP.comma [x, y, z]
    MkRecord fields ->
      encloseSep' PP.lbrace PP.rbrace PP.comma (printField <$> fields)
      where printField :: (Text, PP.Doc) -> PP.Doc
            printField (name, value) = PP.textStrict name <+> "=" <+> value
    MkCustom name items -> PP.sep (PP.textStrict name : items)

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
    Record x -> mconcat $ snd . snd <$> x
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
  elmType :: Proxy a -> ElmType
  toElmValue :: a -> ElmValue
  fromElmValue :: ElmValue -> Maybe a
  default elmType :: (HasElmTypeG (Rep a)) =>
    Proxy a -> ElmType
  elmType _ = elmTypeG (Proxy @(Rep a))
  default toElmValue :: (Generic a, HasElmTypeG (Rep a)) =>
    a -> ElmValue
  toElmValue = toElmValueG . from
  default fromElmValue :: (Generic a, HasElmTypeG (Rep a)) =>
    ElmValue -> Maybe a
  fromElmValue = fmap to . fromElmValueG

instance HasElmType () where
  elmType _ = Fix Unit
  toElmValue () = Fix MkUnit
  fromElmValue =
    \case
      Fix MkUnit -> Just ()
      _ -> Nothing

instance HasElmType Void where
  elmType _ = Fix Never
  toElmValue = absurd
  fromElmValue = const Nothing

instance HasElmType Int32 where
  elmType _ = Fix Int
  toElmValue = Fix . MkInt
  fromElmValue =
    \case
      Fix (MkInt n) -> Just n
      _ -> Nothing

instance HasElmType Text where
  elmType _ = Fix String
  toElmValue = Fix . MkString
  fromElmValue =
    \case
      Fix (MkString n) -> Just n
      _ -> Nothing

instance HasElmType a => HasElmType [a] where
  elmType _ = Fix . List $ elmType (Proxy @a)
  toElmValue = Fix . MkList . fmap toElmValue
  fromElmValue =
    \case
      Fix (MkList xs) -> traverse fromElmValue xs
      _ -> Nothing

-- |
-- Helper class for constructing elm types from generics primitives. The
-- GHC.Generics recommends you create such a class, to allow you to work on
-- kinds * -> * instead of kinds * that the `HasElmType` class works on.
class HasElmTypeG (f :: * -> *) where
  elmTypeG :: Proxy f -> ElmType
  toElmValueG :: f p -> ElmValue
  fromElmValueG :: ElmValue -> Maybe (f p)

instance HasElmTypeG V1 where
  elmTypeG _ = Fix Never
  toElmValueG = \case {}
  fromElmValueG = const Nothing

instance HasElmTypeG U1 where
  elmTypeG _ = Fix Unit
  toElmValueG U1 = Fix MkUnit
  fromElmValueG =
    \case
      Fix MkUnit -> Just U1
      _ -> Nothing

instance HasElmType c => HasElmTypeG (K1 i c) where
  elmTypeG _ = elmType (Proxy @c)
  toElmValueG = toElmValue . unK1
  fromElmValueG = fmap K1 . fromElmValue

instance (HasElmSumG f, HasDataName c) => HasElmTypeG (M1 D c f) where
  elmTypeG _ = mkElmSum (hasDataName (Proxy @c)) (hasElmSumG (Proxy @f))
  toElmValueG = toElmValueSumG . unM1
  fromElmValueG = fmap M1 . fromElmValueSumG

-- |
-- Helper class for constructing sum types.
class HasElmSumG (f :: * -> *) where
  hasElmSumG :: Proxy f -> ElmSum
  toElmValueSumG :: (f p) -> ElmValue
  fromElmValueSumG :: ElmValue -> Maybe (f p)

newtype ElmSum =
  ElmSum [(Text, ElmProduct)]
  deriving (Semigroup, Monoid)

inSum :: ElmValue -> ElmSum -> Bool
inSum (Fix (MkCustom name _)) (ElmSum sum') = name `elem` (fst <$> sum')
inSum _ _ = False

instance HasElmSumG V1 where
  hasElmSumG _ = mempty
  toElmValueSumG = \case {}
  fromElmValueSumG _ = Nothing

instance (HasElmProductG f, HasConstructorName c) => HasElmSumG (M1 C c f) where
  hasElmSumG _ =
    ElmSum [(hasConstructorName (Proxy @c), hasElmProductG (Proxy @f))]
  toElmValueSumG x = Fix $ MkCustom (hasConstructorName (Proxy @c)) params
    where
      params =
        case traverse fst prod of
          Nothing -> values
          Just fieldNames -> [Fix . MkRecord $ zipWith (,) fieldNames values]
      values = toElmValueProductG (unM1 x)
      (ElmProduct prod) = hasElmProductG (Proxy @f)
  fromElmValueSumG params =
    case (params, traverse fst prod) of
      (Fix (MkCustom name [Fix (MkRecord fields)]), Just fieldNames)
        | name == (hasConstructorName (Proxy @c)) -> do
          values <- traverse (flip lookup fields) fieldNames
          rep <- fromElmValueProductG values
          pure $ M1 rep
      (Fix (MkCustom name xs), Nothing)
        | name == (hasConstructorName (Proxy @c)) ->
          M1 <$> fromElmValueProductG xs
      _ -> Nothing
    where
      (ElmProduct prod) = hasElmProductG (Proxy @f)

instance (HasElmSumG f, HasElmSumG g) => HasElmSumG (f :+: g) where
  hasElmSumG _ = hasElmSumG (Proxy @f) <> hasElmSumG (Proxy @g)
  toElmValueSumG =
    \case
      (L1 l) -> toElmValueSumG l
      (R1 r) -> toElmValueSumG r
  fromElmValueSumG custom
    | custom `inSum` hasElmSumG (Proxy @f) = L1 <$> fromElmValueSumG custom
    | custom `inSum` hasElmSumG (Proxy @g) = R1 <$> fromElmValueSumG custom
    | otherwise = Nothing

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
  toElmValueProductG :: f p -> [ElmValue]
  fromElmValueProductG :: [ElmValue] -> Maybe (f p)
  default hasElmProductG :: HasElmTypeG f =>
    Proxy f -> ElmProduct
  hasElmProductG x = ElmProduct [(Nothing, elmTypeG x)]
  default toElmValueProductG :: HasElmTypeG f =>
    f p -> [ElmValue]
  toElmValueProductG = pure . toElmValueG
  default fromElmValueProductG :: HasElmTypeG f =>
    [ElmValue] -> Maybe (f p)
  fromElmValueProductG =
    \case
      [x] -> fromElmValueG x
      _ -> Nothing

newtype ElmProduct =
  ElmProduct [(Maybe Text, ElmType)]
  deriving (Semigroup)

productLength :: ElmProduct -> Int
productLength (ElmProduct xs) = length xs

instance HasElmType c => HasElmProductG (K1 i c)

instance HasElmProductG V1

instance HasElmProductG U1

instance (HasElmProductG f, HasFieldName c) => HasElmProductG (M1 S c f) where
  hasElmProductG _ =
    case hasElmProductG (Proxy @f) of
      ElmProduct fields ->
        ElmProduct $ (map . first) (const (hasFieldName (Proxy @c))) fields
  toElmValueProductG = toElmValueProductG . unM1
  fromElmValueProductG = fmap M1 . fromElmValueProductG

instance (HasElmProductG f, HasElmProductG g) => HasElmProductG (f :*: g) where
  hasElmProductG _ = hasElmProductG (Proxy @f) <> hasElmProductG (Proxy @g)
  toElmValueProductG (x :*: y) = toElmValueProductG x <> toElmValueProductG y
  fromElmValueProductG xs
    | length g == lengthG
    , length f == lengthF =
      (:*:) <$> fromElmValueProductG f <*> fromElmValueProductG g
    | otherwise = Nothing
    where
      lengthF = productLength (hasElmProductG (Proxy @f))
      lengthG = productLength (hasElmProductG (Proxy @g))
      (f, g) = splitAt lengthF xs

mkElmProduct :: ElmProduct -> [ElmType]
mkElmProduct (ElmProduct prod) =
  case traverse fst prod of
    Just names -> [Fix . Record $ zip names values]
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
    _ -> Fix . Record $ zip anonFields (toList values)
      where anonFields = ("field" <>) . Text.pack . show <$> ([1 ..] :: [Int])

-- |
-- Helper class for extracting a name from a generics 'MetaSel type.
class HasFieldName (f :: Meta) where
  hasFieldName :: Proxy f -> Maybe Text

instance HasFieldName ('MetaSel 'Nothing su ss ds) where
  hasFieldName _ = Nothing

instance KnownSymbol n => HasFieldName ('MetaSel ('Just n) su ss ds) where
  hasFieldName _ = Just . Text.pack $ symbolVal (Proxy @n)

-- |
-- Helper class for extracting a name from a generics 'MetaCons type.
class HasConstructorName (a :: Meta) where
  hasConstructorName :: Proxy a -> Text

instance KnownSymbol n => HasConstructorName ('MetaCons n f s) where
  hasConstructorName _ = Text.pack $ symbolVal (Proxy @n)

-- |
-- Helper class for extracting a name from a generics 'MetaData type.
class HasDataName (a :: Meta) where
  hasDataName :: Proxy a -> Text

instance KnownSymbol n => HasDataName ('MetaData n m p nt) where
  hasDataName _ = Text.pack $ symbolVal (Proxy @n)
