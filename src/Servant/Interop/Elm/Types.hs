{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Servant.Interop.Elm.Types
  ( ElmTypeF (..),
    ElmType,
    TypeName,
    ElmTypeDefinition (..),
    toElmTypes,
    sortUserTypes,
    printTypeDefinition,
    printType,
    toUniqueName,
    printName,
  )
where

import Data.Foldable (toList)
import Data.Functor.Foldable (Fix (Fix), cata, unfix, zygo)
import qualified Data.Graph as Graph
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
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
    Defined name params ->
      hangCollapse $ PP.vsep (PP.pretty (printName name) : (uncurry parens <$> params))
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

printName :: TypeName -> Text
printName (TypeName name) =
  case Text.splitOn "|" name of
    [] -> name
    x : _ -> x

printTypeDefinition :: TypeName -> ElmTypeDefinition -> Doc
printTypeDefinition name =
  \case
    Custom constructors ->
      "type" <+> PP.pretty (printName name) <++> printedConstructors
      where
        printedConstructors =
          PP.indent elmIndent . PP.vcat . zipWith (<+>) ("=" : repeat "|") $
            printConstructor <$> toList constructors
    Alias base ->
      "type alias"
        <+> PP.pretty (printName name)
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

toElmTypes :: Wire.UserTypes -> (UserTypes, Wire.Type_ -> ElmType)
toElmTypes wireTypes =
  ( userTypes,
    fromWireType
  )
  where
    (userTypes, elmTypes) =
      traverse (uncurry (toElmType fromWireType))
        $ Map.toList
        $ Wire.unUserTypes wireTypes
    customTypes :: Map Wire.TypeName ElmType
    customTypes =
      Map.fromList $ zip (Map.keys (Wire.unUserTypes wireTypes)) elmTypes
    fromWireType :: Wire.Type_ -> ElmType
    fromWireType =
      cata $ \case
        Wire.Tuple xs -> mkElmTuple $ toList xs
        Wire.Record xs -> Fix . Record $ toList xs
        Wire.User name ->
          case Map.lookup name customTypes of
            Nothing -> error ("Could not find custom type: " <> (show name))
            Just x -> x
        Wire.Void -> Fix Never
        Wire.Int -> Fix Int
        Wire.Float -> Fix Float
        Wire.String -> Fix String
        Wire.Bool -> Fix Bool
        Wire.List x -> Fix $ List x

toElmType ::
  (Wire.Type_ -> ElmType) ->
  Wire.TypeName ->
  Seq (Wire.ConstructorName, Wire.Type_) ->
  (UserTypes, ElmType)
toElmType fromWireType typeName ctors =
  case (Wire.typeConstructor typeName, Seq.sortOn fst ctors) of
    -- We're duck-typing Maybe's. We could restrict this code to only replace
    -- the official `Maybe` type in Haskell's Prelude with an Elm `Maybe`, but
    -- why not give potentially custom `Maybe` definitions the same treatment?
    ("Maybe", [("Just", Fix (Wire.Tuple [a])), ("Nothing", Fix (Wire.Tuple []))]) ->
      ( mempty,
        Fix (Maybe (fromWireType a))
      )
    ("Either", [("Left", Fix (Wire.Tuple [a])), ("Right", Fix (Wire.Tuple [b]))]) ->
      ( mempty,
        Fix (Result (fromWireType a) (fromWireType b))
      )
    ("Result", [("Err", Fix (Wire.Tuple [a])), ("Ok", Fix (Wire.Tuple [b]))]) ->
      ( mempty,
        Fix (Result (fromWireType a) (fromWireType b))
      )
    _ ->
      ( UserTypes $
          Map.singleton
            (toUniqueName typeName)
            (fromWireUserType (toList ctors) fromWireType),
        Fix (Defined (toUniqueName typeName) [])
      )

toUniqueName :: Wire.TypeName -> TypeName
toUniqueName Wire.TypeName {Wire.typeConstructor, Wire.fromModule, Wire.parameters} =
  TypeName $
    Text.intercalate
      "|"
      [ typeConstructor,
        mconcat (typeConstructor : parameters),
        mconcat (fromModule : typeConstructor : parameters)
      ]

fromWireUserType ::
  [(Wire.ConstructorName, Wire.Type_)] ->
  (Wire.Type_ -> ElmType) ->
  ElmTypeDefinition
fromWireUserType [] _ = Alias (Fix Never)
fromWireUserType (c : cs) fromWireType = Custom . (fmap . fmap) mkConstructors $ c :| cs
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
