{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Servant.Interop.Elm.Types
  ( ElmTypeF,
    ElmTypeF' (..),
    ElmType,
    ElmTypeDefinition (..),
    sortUserTypes,
    fromWireUserTypes,
    printTypeDefinition,
    printType,
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
import Data.Text (Text)
import qualified Data.Text as Text
import Servant.Interop.Elm.Print
import Text.PrettyPrint.Leijen.Text ((<+>))
import qualified Text.PrettyPrint.Leijen.Text as PP
import qualified Wire

type ElmTypeF a = ElmTypeF' Wire.FieldName a

data ElmTypeF' s a
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
  | Record [(s, a)]
  | Lambda
      a
      a
  | Defined Wire.TypeName
  deriving (Functor)

type ElmType = Fix (ElmTypeF' Wire.FieldName)

newtype UserTypes
  = UserTypes
      { unUserTypes :: Map Wire.TypeName ElmTypeDefinition
      }
  deriving (Monoid, Semigroup)

data ElmTypeDefinition
  = Custom (NonEmpty (Wire.ConstructorName, [ElmType]))
  | Alias ElmType

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
    Result x y -> x <> y
    Tuple2 x y -> x <> y
    Tuple3 x y z -> x <> y <> z
    Record x -> foldMap snd x
    Defined n -> [n]
    Lambda x y -> x <> y

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
    Result (ai, i) (aj, j) -> "Result" <+> parens ai i <+> parens aj j
    Tuple2 (_, i) (_, j) -> encloseSep' PP.lparen PP.rparen PP.comma [i, j]
    Tuple3 (_, i) (_, j) (_, k) ->
      encloseSep' PP.lparen PP.rparen PP.comma [i, j, k]
    Record xs ->
      encloseSep' PP.lbrace PP.rbrace PP.comma (printRecordField <$> xs)
    Defined name -> PP.textStrict $ unqualifiedName name
    Lambda (ai, i) (_, o) -> idoc <+> "->" <+> o
      where
        -- We only need to parenthesize the input argument if it is a
        -- lambda function itself.
        idoc =
          case ai of
            SingleWord -> i
            MultipleWord -> i
            MultipleWordLambda -> PP.parens i

printTypeDefinition :: Wire.TypeName -> ElmTypeDefinition -> PP.Doc
printTypeDefinition name =
  \case
    Custom constructors ->
      "type" <+> PP.textStrict (unqualifiedName name) <++> printedConstructors
      where
        printedConstructors =
          PP.indent elmIndent . PP.vcat . zipWith (<+>) ("=" : repeat "|") $
            printConstructor <$> toList constructors
    Alias base ->
      "type alias"
        <+> PP.textStrict (unqualifiedName name)
        <+> "=" <++> PP.indent elmIndent (printType base)

unqualifiedName :: Wire.TypeName -> Text
unqualifiedName = Wire.typeConstructor

printConstructor :: (Wire.ConstructorName, [ElmType]) -> PP.Doc
printConstructor (name, params) =
  PP.nest
    elmIndent
    ( PP.sep
        (PP.textStrict (Wire.unConstructorName name) : (printParam <$> params))
    )
  where
    printParam :: ElmType -> PP.Doc
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
    Defined _ -> SingleWord
    Lambda _ _ -> MultipleWordLambda

printRecordField :: (Wire.FieldName, (a, PP.Doc)) -> PP.Doc
printRecordField (k, (_, v)) =
  hangCollapse $ PP.textStrict (Wire.unFieldName k) <+> ":" <++> v

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
    replacements :: Map Wire.TypeName ElmType
    replacements = elmCoreTypeReplacements userTypes
    replaceInTypeDefinition :: ElmTypeDefinition -> ElmTypeDefinition
    replaceInTypeDefinition (Alias x) = Alias (replace x)
    replaceInTypeDefinition (Custom ctors) =
      Custom ((fmap . fmap . fmap) replace ctors)
    replace :: ElmType -> ElmType
    replace =
      cata $ \case
        x@(Defined name) -> fromMaybe (Fix x) $ Map.lookup name replacements
        x -> Fix x

fromWireUserTypes :: Wire.UserTypes -> (UserTypes, ElmType -> ElmType)
fromWireUserTypes =
  useElmCoreTypes
    . UserTypes
    . fmap (fromWireUserType . toList)
    . Wire.unUserTypes

elmCoreTypeReplacements :: UserTypes -> Map Wire.TypeName ElmType
elmCoreTypeReplacements =
  Map.mapMaybeWithKey replaceWithElmCoreType . unUserTypes

replaceWithElmCoreType :: Wire.TypeName -> ElmTypeDefinition -> Maybe ElmType
replaceWithElmCoreType _ (Alias _) = Nothing
replaceWithElmCoreType typeName (Custom typeDef) =
  case (Wire.typeConstructor typeName, orderedConstructors) of
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
        Fix (Wire.Tuple params) -> toList $ fmap fromWireType params
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

sortUserTypes :: UserTypes -> [(Wire.TypeName, ElmTypeDefinition)]
sortUserTypes =
  reverse
    . Graph.flattenSCCs
    . Graph.stronglyConnComp
    . fmap toNode
    . Map.toList
    . unUserTypes
  where
    toNode ::
      (Wire.TypeName, ElmTypeDefinition) ->
      ((Wire.TypeName, ElmTypeDefinition), Wire.TypeName, [Wire.TypeName])
    toNode (name, t) = ((name, t), name, namesInTypeDefinition t)
