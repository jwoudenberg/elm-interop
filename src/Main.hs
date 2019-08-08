{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Monad.Free (Free(Free))
import Data.Coerce (coerce)
import Data.Foldable (toList)
import Data.Functor.Foldable (Fix(Fix), cata, futu, para, unfix, zygo)
import Data.Int (Int32)
import Data.List.NonEmpty (NonEmpty((:|)))
import Data.Proxy (Proxy)
import Data.Text (Text)
import Text.PrettyPrint.Leijen.Text ((<+>))

import qualified Data.Text as Text
import qualified Text.PrettyPrint.Leijen.Text as PP
import qualified Wire

-- |
-- Small test of functionality in this library. Will be removed before release.
main :: IO ()
main = do
  putStrLn "Hi"

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
-- Get the Elm-representation of a type.
elmType :: Wire.Elm a => Proxy a -> ElmType
elmType = futu go . Wire.wireType
  where
    go :: Wire.WireType -> ElmTypeF (Free ElmTypeF Wire.WireType)
    go =
      \case
        Wire.Int -> Int
        Wire.Float -> Float
        Wire.String -> String
        Wire.Sum _ [] -> Never
        Wire.Sum name (x:xs) -> Defined $ Custom name constructors
          where constructors = fmap toElmConstructor (x :| xs)
                toElmConstructor = fmap toParamsList . coerce

-- |
-- Build a params list for a constructor.
toParamsList :: [(Text, a)] -> [Free ElmTypeF a]
toParamsList params =
  case traverse (nonNull . fst) params of
    Just names -> pure . Free . fmap pure . Record $ zip names (map snd params)
    -- ^ All params are named. This is a record.
    --
    --     type Thing = Constructor { number : Int, message : String }
    Nothing -> pure . snd <$> params
    -- ^ At least one param is not named. This is an argument list.
    --
    --     type Thing = Constructor Int String

nonNull :: Text -> Maybe Text
nonNull =
  \case
    "" -> Nothing
    x -> Just x

-- |
-- Build an Elm type representing a 'tuple' of different types.
mkElmTuple :: [ElmType] -> ElmType
mkElmTuple values =
  case values of
    [] -> Fix Unit
    -- ^ An empty tuple is isomporphic with `()`.
    [x] -> x
    -- ^ A single-valued tuple doesn't need any sort of wrapping.
    [x, y] -> Fix $ Tuple2 x y
    -- ^ A 2-tuple. Example: `(Int, Text)`.
    [x, y, z] -> Fix $ Tuple3 x y z
    -- ^ A 3-tuple. Example: `(Int, Text, Bool)`.
    _ -> Fix . Record $ zip anonFields (toList values)
    -- ^ Elm only has tuples with 2 or 3 elements. If we have more values
    -- than that we have to use a record.
      where anonFields = ("field" <>) . Text.pack . show <$> ([1 ..] :: [Int])
