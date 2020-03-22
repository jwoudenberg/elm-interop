{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Servant.Interop.Elm.Types
  ( ElmTypeF (..),
    ElmType,
    TypeName (..),
    ElmTypeDefinition (..),
    sortUserTypes,
    fromWireUserTypes,
    printTypeDefinition,
    printType,
    fromWireType,
    toElmTypeName,
  )
where

import Data.Foldable (toList)
import Data.Functor.Foldable (Fix (Fix), cata, unfix, zygo)
import qualified Data.Graph as Graph
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.List.NonEmpty as NonEmpty
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.String (IsString)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Prettyprint.Doc ((<+>))
import qualified Data.Text.Prettyprint.Doc as PP
import Servant.Interop.Elm.Print
import qualified Wire

type ElmType = Fix ElmTypeF

data ElmTypeF a
  = Unit
  | Never
  | Bool
  | Int
  | Float
  | String
  | List a
  | Maybe a
  | Result
      a
      a
  | Tuple2
      a
      a
  | Tuple3
      a
      a
      a
  | Record [(Wire.FieldName, a)]
  | Lambda
      a
      a
  | Cmd a
  | Defined TypeName [a]
  deriving (Functor)

newtype TypeName = TypeName Text
  deriving (Eq, IsString, Ord)

newtype UserTypes
  = UserTypes
      { unUserTypes :: Map TypeName ElmTypeDefinition
      }
  deriving (Monoid, Semigroup)

data ElmTypeDefinition
  = Custom (NonEmpty (Wire.ConstructorName, [ElmType]))
  | Alias ElmType

namesInTypeDefinition :: ElmTypeDefinition -> [TypeName]
namesInTypeDefinition =
  \case
    Alias t -> namesInType t
    Custom xs -> foldMap namesInType . foldMap snd $ toList xs

namesInType :: ElmType -> [TypeName]
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
    Result x y -> x <> y
    Tuple2 x y -> x <> y
    Tuple3 x y z -> x <> y <> z
    Record x -> foldMap snd x
    Defined n params -> n : mconcat params
    Cmd x -> x
    Lambda x y -> x <> y

printType :: ElmType -> Doc
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
    Result (ai, i) (aj, j) -> "Result" <+> parens ai i <+> parens aj j
    Tuple2 (_, i) (_, j) -> encloseSep' PP.lparen PP.rparen PP.comma [i, j]
    Tuple3 (_, i) (_, j) (_, k) ->
      encloseSep' PP.lparen PP.rparen PP.comma [i, j, k]
    Record xs ->
      encloseSepUngrouped PP.lbrace PP.rbrace PP.comma (printRecordField <$> xs)
    Defined (TypeName name) params ->
      hangCollapse $ PP.vsep (PP.pretty name : (uncurry parens <$> params))
    Cmd (a, i) -> "Cmd" <+> parens a i
    Lambda (ai, i) (_, o) -> idoc <++> "->" <+> o
      where
        -- We only need to parenthesize the input argument if it is a
        -- lambda function itself.
        idoc =
          case ai of
            SingleWord -> i
            MultipleWord -> i
            MultipleWordLambda -> PP.parens i

printTypeDefinition :: TypeName -> ElmTypeDefinition -> Doc
printTypeDefinition (TypeName name) =
  \case
    Custom constructors ->
      "type" <+> PP.pretty name <++> printedConstructors
      where
        printedConstructors =
          PP.indent elmIndent . PP.vcat . zipWith (<+>) ("=" : repeat "|") $
            printConstructor <$> toList constructors
    Alias base ->
      "type alias"
        <+> PP.pretty name
        <+> "=" <++> PP.indent elmIndent (printType base)

printConstructor :: (Wire.ConstructorName, [ElmType]) -> Doc
printConstructor (name, params') =
  PP.nest
    elmIndent
    ( PP.sep
        (PP.pretty (Wire.unConstructorName name) : (printParam <$> params'))
    )
  where
    printParam :: ElmType -> Doc
    printParam t = parens (appearance (unfix t)) (printType t)

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
    Result _ _ -> MultipleWord
    Tuple2 _ _ -> SingleWord
    Tuple3 _ _ _ -> SingleWord
    Record _ -> SingleWord
    Defined _ params -> if null params then SingleWord else MultipleWord
    Cmd _ -> MultipleWord
    Lambda _ _ -> MultipleWordLambda

printRecordField :: (Wire.FieldName, (a, Doc)) -> Doc
printRecordField (k, (_, v)) =
  hangCollapse $ PP.pretty (Wire.unFieldName k) <+> ":" <++> v

-- |
-- The wire format is intentionally very limited and does not include many types
-- types that are in `elm-core`, such as `Maybe a` and `Result err ok`. When the
-- user generates Elm code for a Haskell `Maybe a` we'd still like that to be
-- mapped onto the Elm equivalent. This code ensures it does.
useElmCoreTypes :: UserTypes -> (UserTypes, ElmType -> ElmType)
useElmCoreTypes userTypes =
  ( UserTypes . fmap replaceInTypeDefinition $
      Map.withoutKeys (unUserTypes userTypes) (Map.keysSet replacements),
    replace
  )
  where
    replacements :: Map TypeName ElmType
    replacements = elmCoreTypeReplacements userTypes
    replaceInTypeDefinition :: ElmTypeDefinition -> ElmTypeDefinition
    replaceInTypeDefinition (Alias x) = Alias (replace x)
    replaceInTypeDefinition (Custom ctors) =
      Custom ((fmap . fmap . fmap) replace ctors)
    replace :: ElmType -> ElmType
    replace =
      cata $ \case
        x@(Defined name _) -> fromMaybe (Fix x) $ Map.lookup name replacements
        x -> Fix x

fromWireUserTypes :: Wire.UserTypes -> (UserTypes, ElmType -> ElmType)
fromWireUserTypes =
  useElmCoreTypes
    . UserTypes
    . Map.mapKeys toElmTypeName
    . fmap (fromWireUserType . toList)
    . Wire.unUserTypes

elmCoreTypeReplacements :: UserTypes -> Map TypeName ElmType
elmCoreTypeReplacements =
  Map.mapMaybeWithKey replaceWithElmCoreType . unUserTypes

replaceWithElmCoreType :: TypeName -> ElmTypeDefinition -> Maybe ElmType
replaceWithElmCoreType _ (Alias _) = Nothing
replaceWithElmCoreType typeName (Custom typeDef) =
  case (typeName, orderedConstructors) of
    ("Maybe", [("Just", [a]), ("Nothing", [])]) -> Just (Fix $ Maybe a)
    ("Either", [("Left", [a]), ("Right", [b])]) -> Just (Fix $ Result a b)
    -- There's not a `Result` type in the Haskell standard library, but if you
    -- would create one, you'd expect it to map to the Elm `Result` type.
    ("Result", [("Err", [a]), ("Ok", [b])]) -> Just (Fix $ Result a b)
    _ -> Nothing
  where
    orderedConstructors :: [(Wire.ConstructorName, [ElmType])]
    orderedConstructors = toList $ NonEmpty.sortWith fst typeDef

fromWireUserType :: [(Wire.ConstructorName, Wire.Type_)] -> ElmTypeDefinition
fromWireUserType [] = Alias (Fix Never)
fromWireUserType (c : cs) = Custom . (fmap . fmap) mkConstructors $ c :| cs
  where
    mkConstructors :: Wire.Type_ -> [ElmType]
    mkConstructors =
      \case
        Fix (Wire.Tuple params') -> toList $ fmap fromWireType params'
        -- We don't expect anything but a product here, but should we get one
        -- we'll assume it's a single parameter to the constructor.
        Fix (Wire.Record fields) ->
          pure . Fix . Record . (fmap . fmap) fromWireType $ toList fields
        param -> [fromWireType param]

fromWireType :: Wire.Type_ -> ElmType
fromWireType =
  cata $ \case
    Wire.Tuple xs -> mkElmTuple $ toList xs
    Wire.Record xs -> Fix . Record $ toList xs
    Wire.User name -> Fix $ Defined (toElmTypeName name) []
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
    -- A 2-tuple. Example: `(Int, Text)`.
    [x, y] -> Fix $ Tuple2 (x) (y)
    -- A 3-tuple. Example: `(Int, Text, Bool)`.
    [x, y, z] -> Fix $ Tuple3 (x) (y) (z)
    -- Elm only has tuples with 2 or 3 elements. If we have more values
    -- than that we have to use a record.
    xs -> Fix . Record $ zip anonFields xs
      where
        anonFields =
          Wire.FieldName . ("field" <>) . Text.pack . show
            <$> ([1 ..] :: [Int])

sortUserTypes :: UserTypes -> [(TypeName, ElmTypeDefinition)]
sortUserTypes =
  reverse
    . Graph.flattenSCCs
    . Graph.stronglyConnComp
    . fmap toNode
    . Map.toList
    . unUserTypes
  where
    toNode ::
      (TypeName, ElmTypeDefinition) ->
      ((TypeName, ElmTypeDefinition), TypeName, [TypeName])
    toNode (name, t) = ((name, t), name, namesInTypeDefinition t)

toElmTypeName :: Wire.TypeName -> TypeName
toElmTypeName name =
  TypeName $
    if Wire.fromModule name `elem` knownModules
      then qualifiedName name
      else unqualifiedName name
  where
    unqualifiedName :: Wire.TypeName -> Text
    unqualifiedName name' = Wire.typeConstructor name' <> mconcat (Wire.parameters name')
    qualifiedName :: Wire.TypeName -> Text
    qualifiedName name' =
      Wire.fromModule name' <> "." <> unqualifiedName name'
    knownModules :: [Text]
    knownModules =
      [ "Http",
        "Json.Decode",
        "Json.Encode"
      ]
