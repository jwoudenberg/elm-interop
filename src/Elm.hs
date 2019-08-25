{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Elm
  ( Wire.Rep
  , ElmType
  , elmType
  , UserTypes
  , printModule
  , printValue
  ) where

import Data.Bifunctor (bimap)
import Data.Foldable (toList)
import Data.Functor.Foldable (Fix(Fix), cata, unfix, zygo)
import Data.Int (Int32)
import Data.List (intersperse)
import Data.List.NonEmpty (NonEmpty((:|)))
import Data.Map (Map)
import Data.Proxy (Proxy)
import Data.Text (Text)
import Text.PrettyPrint.Leijen.Text ((<+>))

import qualified Data.Graph as Graph
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Text.PrettyPrint.Leijen.Text as PP
import qualified Wire

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
  | Record [(Wire.FieldName, a)]
  | Lambda a
           a
  | Defined Wire.TypeName
  deriving (Functor)

type ElmType = Fix ElmTypeF

newtype UserTypes = UserTypes
  { unUserTypes :: Map Wire.TypeName ElmTypeDefinition
  } deriving (Monoid, Semigroup)

data ElmTypeDefinition
  = Custom (NonEmpty (Wire.ConstructorName, [ElmType]))
  | Alias ElmType

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

printModule :: UserTypes -> Text
printModule =
  printDoc .
  PP.vcat .
  intersperse PP.linebreak . fmap (uncurry printTypeDefinition) . sortUserTypes

sortUserTypes :: UserTypes -> [(Wire.TypeName, ElmTypeDefinition)]
sortUserTypes =
  reverse .
  Graph.flattenSCCs .
  Graph.stronglyConnComp . fmap toNode . Map.toList . unUserTypes
  where
    toNode ::
         (Wire.TypeName, ElmTypeDefinition)
      -> ((Wire.TypeName, ElmTypeDefinition), Wire.TypeName, [Wire.TypeName])
    toNode (name, t) = ((name, t), name, namesInTypeDefinition t)

namesInTypeDefinition :: ElmTypeDefinition -> [Wire.TypeName]
namesInTypeDefinition =
  \case
    Alias t -> namesInType t
    Custom xs -> foldMap namesInType . foldMap snd $ toList xs

namesInType :: ElmType -> [Wire.TypeName]
namesInType =
  cata $ \case
    Unit -> mempty
    Never -> mempty
    Bool -> mempty
    Int -> mempty
    Float -> mempty
    String -> mempty
    List x -> x
    Maybe x -> x
    Tuple2 x y -> x <> y
    Tuple3 x y z -> x <> y <> z
    Record x -> foldMap snd x
    Defined n -> [n]
    Lambda x y -> x <> y

printDoc :: PP.Doc -> Text
printDoc = PP.displayTStrict . PP.renderPretty 1 80

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
    Defined name -> PP.textStrict $ unqualifiedName name
    Lambda (ai, i) (_, o) -> idoc <+> "->" <+> o
      -- |
      -- We only need to parenthesize the input argument if it is a
      -- lambda function itself.
      where idoc =
              case ai of
                SingleWord -> i
                MultipleWord -> i
                MultipleWordLambda -> PP.parens i

printTypeDefinition :: Wire.TypeName -> ElmTypeDefinition -> PP.Doc
printTypeDefinition name =
  \case
    Custom constructors ->
      "type" <+> PP.textStrict (unqualifiedName name) <++> printedConstructors
      where printedConstructors =
              PP.indent elmIndent . PP.vcat . zipWith (<+>) ("=" : repeat "|") $
              printConstructor <$> toList constructors
    Alias base ->
      "type alias" <+>
      PP.textStrict (unqualifiedName name) <+>
      "=" <++> PP.indent elmIndent (printType base)

unqualifiedName :: Wire.TypeName -> Text
unqualifiedName = Wire.typeConstructor

printConstructor :: (Wire.ConstructorName, [ElmType]) -> PP.Doc
printConstructor (name, params) =
  PP.nest
    elmIndent
    (PP.sep
       (PP.textStrict (Wire.unConstructorName name) : (printParam <$> params)))
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

-- |
-- Replacement for the `PP.<$>` operator, which we use for `fmap` instead.
infixr 5 <++>

(<++>) :: PP.Doc -> PP.Doc -> PP.Doc
(<++>) = (PP.<$>)

elmIndent :: Int
elmIndent = 4

-- |
-- Version of `encloseSep` that puts the closing delimiter on a new line, and
-- adds a space between the separator and the content.
--
-- Used for printing lists and records in a fashion compatible with elm-format.
encloseSep' :: PP.Doc -> PP.Doc -> PP.Doc -> [PP.Doc] -> PP.Doc
encloseSep' left right sp ds =
  case ds of
    [] -> left <> right
    _ -> PP.vcat entries <++> right
      where entries = zipWith (<+>) (left : repeat sp) ds

-- |
-- Switch between hanging formatting or single-line formatting.
--
-- Hanging:
--
--     line1
--        line2
--        line3
--
-- Single-line notation:
--
--     line1 line2 line3
--
hangCollapse :: PP.Doc -> PP.Doc
hangCollapse = PP.nest elmIndent . PP.group

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

printRecordField :: (Wire.FieldName, (a, PP.Doc)) -> PP.Doc
printRecordField (k, (_, v)) =
  hangCollapse $ PP.textStrict (Wire.unFieldName k) <+> ":" <++> v

-- |
-- Get the Elm-representation of a type. The Elm representation might make
-- reference to custom types which you get as well.
elmType :: Wire.Rep a => Proxy a -> (UserTypes, ElmType)
elmType = bimap fromWireUserTypes fromWireType . Wire.wireType

fromWireUserTypes :: Wire.UserTypes -> UserTypes
fromWireUserTypes =
  UserTypes . fmap (fromWireUserType . toList) . Wire.unUserTypes

fromWireUserType ::
     [(Wire.ConstructorName, Wire.PrimitiveType)] -> ElmTypeDefinition
fromWireUserType [] = Alias (Fix Never)
fromWireUserType (c:cs) = Custom . (fmap . fmap) mkConstructors $ c :| cs
  where
    mkConstructors :: Wire.PrimitiveType -> [ElmType]
    mkConstructors =
      \case
        Fix (Wire.Tuple params) -> toList $ fmap fromWireType params
        Fix (Wire.Record fields) ->
          pure . Fix . Record . (fmap . fmap) fromWireType $ toList fields
        -- | We don't expect anything but a product here, but should we get one
        -- we'll assume it's a single parameter to the constructor.
        param -> [fromWireType param]

fromWireType :: Wire.PrimitiveType -> ElmType
fromWireType =
  cata $ \case
    Wire.Tuple xs -> mkElmTuple $ toList xs
    Wire.Record xs -> Fix . Record $ toList xs
    Wire.User name -> Fix $ Defined name
    Wire.Void -> Fix Never
    Wire.Int -> Fix Int
    Wire.Float -> Fix Float
    Wire.String -> Fix String
    Wire.Bool -> Fix Bool
    Wire.List x -> Fix $ List x

-- |
-- Build an Elm type representing a 'tuple' of different types.
mkElmTuple :: [ElmType] -> ElmType
mkElmTuple values =
  case values of
    [] -> Fix Unit
    [x] -> x
    [x, y] -> Fix $ Tuple2 (x) (y)
    -- ^ A 2-tuple. Example: `(Int, Text)`.
    [x, y, z] -> Fix $ Tuple3 (x) (y) (z)
    -- ^ A 3-tuple. Example: `(Int, Text, Bool)`.
    xs -> Fix . Record $ zip anonFields xs
    -- ^ Elm only has tuples with 2 or 3 elements. If we have more values
    -- than that we have to use a record.
      where anonFields =
              Wire.FieldName . ("field" <>) . Text.pack . show <$>
              ([1 ..] :: [Int])
