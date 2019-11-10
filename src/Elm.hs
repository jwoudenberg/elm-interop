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
import Data.Functor.Foldable (Fix(Fix), cata, unfix, unfix, zygo)
import Data.Int (Int32)
import Data.List (intersperse)
import Data.List.NonEmpty (NonEmpty((:|)))
import Data.Map (Map)
import Data.Maybe (fromMaybe)
import Data.Proxy (Proxy)
import Data.String (IsString)
import Data.Text (Text)
import Text.PrettyPrint.Leijen.Text ((<+>))

import qualified Data.Graph as Graph
import qualified Data.List.NonEmpty as NonEmpty
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
  | Result a
           a
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
  | MkLambda [Pattern]
             a
  | MkFnCall VariableName
             [a]
  | MkCase a
           [(Pattern, a)]
  | MkVar VariableName
  deriving (Functor)

mkUnit :: ElmValue
mkUnit = Fix MkUnit

_mkBool :: Bool -> ElmValue
_mkBool = Fix . MkBool

_mkInt :: Int32 -> ElmValue
_mkInt = Fix . MkInt

_mkFloat :: Double -> ElmValue
_mkFloat = Fix . MkFloat

mkString :: Text -> ElmValue
mkString = Fix . MkString

mkList :: [ElmValue] -> ElmValue
mkList = Fix . MkList

_mKMaybe :: Maybe ElmValue -> ElmValue
_mKMaybe = Fix . MKMaybe

mkTuple2 :: ElmValue -> ElmValue -> ElmValue
mkTuple2 a b = Fix $ MkTuple2 a b

_mkTuple3 :: ElmValue -> ElmValue -> ElmValue -> ElmValue
_mkTuple3 a b c = Fix $ MkTuple3 a b c

_mkRecord :: [(Text, ElmValue)] -> ElmValue
_mkRecord = Fix . MkRecord

_mkCustom :: Text -> [ElmValue] -> ElmValue
_mkCustom name constructors = Fix $ MkCustom name constructors

mkLambda :: [Pattern] -> ElmValue -> ElmValue
mkLambda args body = Fix $ MkLambda args body

mkFnCall :: VariableName -> [ElmValue] -> ElmValue
mkFnCall name body = Fix $ MkFnCall name body

mkCase :: ElmValue -> [(Pattern, ElmValue)] -> ElmValue
mkCase matched branches = Fix $ MkCase matched branches

mkVar :: VariableName -> ElmValue
mkVar = Fix . MkVar

newtype VariableName = VariableName
  { unVariableName :: Text
  } deriving (IsString)

newtype ConstructorName = ConstructorName
  { unConstructorName :: Text
  } deriving (IsString)

-- | A pattern to match on, in case statements or function arguments.
data Pattern
  = Variable VariableName
  | Match ConstructorName
          [Pattern]

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
    Result x y -> x <> y
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
    Result (ai, i) (aj, j) -> "Result" <+> parens ai i <+> parens aj j
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
    MkLambda params body ->
      "\\" <+> PP.sep (printPattern <$> params) <+> "->" <+> body
    MkFnCall name args -> PP.sep (printVariableName name : args)
    MkCase matched branches ->
      "case" <+> matched <+> "of" <> PP.vsep (uncurry printBranch <$> branches)
      where printBranch :: Pattern -> PP.Doc -> PP.Doc
            printBranch pattern body = printPattern pattern <+> "->" <+> body
    MkVar name -> printVariableName name

printPattern :: Pattern -> PP.Doc
printPattern (Variable name) = printVariableName name
printPattern (Match ctor vars) =
  PP.sep $ (printConstructorName ctor) : (printPattern <$> vars)

printVariableName :: VariableName -> PP.Doc
printVariableName = PP.textStrict . unVariableName

printConstructorName :: ConstructorName -> PP.Doc
printConstructorName = PP.textStrict . unConstructorName

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
    Result _ _ -> MultipleWord
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
elmType = useElmCoreTypes . bimap fromWireUserTypes fromWireType . Wire.wireType

_elmEncoder :: ElmType -> ElmValue
_elmEncoder =
  cata $ \case
    Unit -> mkFnCall "Basics.always" [mkUnit]
    Never -> mkFnCall "Basics.never" []
    Bool -> mkFnCall "Json.Encode.bool" []
    Int -> mkFnCall "Json.Encode.int" []
    Float -> mkFnCall "Json.Encode.float" []
    String -> mkFnCall "Json.Encode.string" []
    List a -> mkFnCall "Json.Encode.list" [a]
    Maybe a -> customTypeEncoder [("Nothing", []), ("Just", [a])]
    Result err ok -> customTypeEncoder [("Err", [err]), ("Ok", [ok])]
    Tuple2 _a _b -> undefined
    Tuple3 _a _b _c -> undefined
    Record _fields -> undefined
    Lambda _i _o -> undefined
    Defined _name -> undefined

customTypeEncoder :: [(ConstructorName, [ElmValue])] -> ElmValue
customTypeEncoder ctors =
  mkLambda [Variable "x"] . mkCase (mkVar "x") $
  uncurry constructorEncoder <$> ctors

constructorEncoder :: ConstructorName -> [ElmValue] -> (Pattern, ElmValue)
constructorEncoder name paramEncoders =
  let vars =
        take (length paramEncoders) $
        (VariableName . ("x" <>) . Text.pack . show) <$> ([1 ..] :: [Int])
   in ( Match name (Variable <$> vars)
      , recordEncoder
          [ ( "ctor"
            , mkFnCall "Json.Encode.string" [mkString (unConstructorName name)])
          , ( "value"
            , mkList $
              zipWith
                (\param encoder -> mkFnCall "Basics.<|" [encoder, mkVar param])
                vars
                paramEncoders)
          ])

recordEncoder :: [(Text, ElmValue)] -> ElmValue
recordEncoder fields =
  mkFnCall "object" [mkList $ uncurry fieldEncoder <$> fields]
  where
    fieldEncoder :: Text -> ElmValue -> ElmValue
    fieldEncoder name = mkTuple2 (mkString name)

-- |
-- The wire format is intentionally very limited and does not include many types
-- types that are in `elm-core`, such as `Maybe a` and `Result err ok`. When the
-- user generates Elm code for a Haskell `Maybe a` we'd still like that to be
-- mapped onto the Elm equivalent. This code ensures it does.
useElmCoreTypes :: (UserTypes, ElmType) -> (UserTypes, ElmType)
useElmCoreTypes (userTypes, type_) =
  ( UserTypes . fmap replaceInTypeDefinition $
    Map.withoutKeys (unUserTypes userTypes) (Map.keysSet replacements)
  , replace type_)
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

fromWireUserTypes :: Wire.UserTypes -> UserTypes
fromWireUserTypes =
  UserTypes . fmap (fromWireUserType . toList) . Wire.unUserTypes

elmCoreTypeReplacements :: UserTypes -> Map Wire.TypeName ElmType
elmCoreTypeReplacements =
  Map.mapMaybeWithKey replaceWithElmCoreType . unUserTypes

replaceWithElmCoreType :: Wire.TypeName -> ElmTypeDefinition -> Maybe ElmType
replaceWithElmCoreType _ (Alias _) = Nothing
replaceWithElmCoreType typeName (Custom typeDef) =
  case (Wire.typeConstructor typeName, orderedConstructors) of
    ("Maybe", [("Just", [a]), ("Nothing", [])]) -> Just (Fix $ Maybe a)
    ("Either", [("Left", [a]), ("Right", [b])]) -> Just (Fix $ Result a b)
    ("Result", [("Err", [a]), ("Ok", [b])]) -> Just (Fix $ Result a b)
    -- ^ There's not a `Result` type in the Haskell standard library, but if you
    -- would create one, you'd expect it to map to the Elm `Result` type.
    _ -> Nothing
  where
    orderedConstructors :: [(Wire.ConstructorName, [ElmType])]
    orderedConstructors = toList $ NonEmpty.sortWith fst typeDef

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
