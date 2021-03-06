{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Servant.Interop.Elm.Values
  ( ElmFunction (..),
    ElmType,
    ElmValue,
    ElmValueF (..),
    Pattern,
    PatternF (..),
    Variable,
    varName,
    fromVarName,
    printFunction,
    anyType,
    unit,
    lambda,
    fn1,
    fn2,
    fn3,
    fn4,
    fn5,
    fn6,
    fn7,
    fn8,
    fn9,
    (<|),
    (|>),
    mkCase,
    matchVar,
    matchString,
    matchInt,
    matchTuple2,
    matchTuple3,
    matchRecordN,
    matchCtor1Pattern,
    matchCtor0,
    matchCtor1,
    matchCtorN,
    p0,
    list,
    string,
    int,
    var,
    v,
    tuple,
    tuple3,
    Record,
    emptyRecord,
    addField,
    mkRecord,
    ifThenElse,

    -- * Library functions

    -- ** Basics
    _Just,
    _Nothing,
    _Err,
    _Ok,
    _always,
    _identity,
    _never,
    _String_fromInt,
    _String_fromFloat,

    -- ** String
    _String_join,
    _String_concat,

    -- ** Tuple
    _Tuple_pair,

    -- ** List
    _List_map,
    _List_concat,
    _List_intersperse,

    -- ** Json.Encode
    _Json_Encode_encode,
    _Json_Encode_list,
    _Json_Encode_bool,
    _Json_Encode_int,
    _Json_Encode_float,
    _Json_Encode_string,
    _Json_Encode_object,
    _Json_Encode_null,

    -- ** Json.Decocode
    _Json_Decode_string,
    _Json_Decode_bool,
    _Json_Decode_int,
    _Json_Decode_float,
    _Json_Decode_nullable,
    _Json_Decode_list,
    _Json_Decode_array,
    _Json_Decode_dict,
    _Json_Decode_keyValuePairs,
    _Json_Decode_field,
    _Json_Decode_at,
    _Json_Decode_index,
    _Json_Decode_maybe,
    _Json_Decode_oneOf,
    _Json_Decode_decodeString,
    _Json_Decode_decodeValue,
    _Json_Decode_map,
    _Json_Decode_map2,
    _Json_Decode_map3,
    _Json_Decode_map4,
    _Json_Decode_map5,
    _Json_Decode_map6,
    _Json_Decode_map7,
    _Json_Decode_map8,
    _Json_Decode_lazy,
    _Json_Decode_value,
    _Json_Decode_null,
    _Json_Decode_succeed,
    _Json_Decode_fail,
    _Json_Decode_andThen,

    -- * Url.Builder
    _Url_Builder_absolute,
    _Url_Builder_string,
    _Url_Builder_int,

    -- * Http
    _Http_request,
    _Http_header,
    _Http_emptyBody,
    _Http_jsonBody,
    _Http_expectWhatever,
    _Http_expectJson,

    -- * Phantom types
    QueryParameter,
    Body,
    Decoder,
    Error,
    Expect,
    Header,
    Array,
    Value,
    Result,
  )
where

import Control.Comonad (extract)
import Control.Comonad.Cofree (Cofree ((:<)))
import Data.Functor.Foldable (Fix (Fix), cata, histo)
import Data.Int (Int32)
import Data.String (IsString (fromString))
import Data.Text (Text)
import Data.Text.Prettyprint.Doc ((<+>))
import qualified Data.Text.Prettyprint.Doc as PP
import Servant.Interop.Elm.Print
import Servant.Interop.Elm.Types (ElmType, printType)

-- |
-- A type representing a value in Elm.
data ElmValueF a
  = MkUnit
  | MkBool Bool
  | MkInt Int32
  | MkFloat Double
  | MkString Text
  | MkList [a]
  | MKMaybe (Maybe a)
  | MkTuple2 a a
  | MkTuple3 a a a
  | MkRecord [(Text, a)]
  | MkLambda (Fix PatternF) a
  | MkApply a a
  | MkVar Text
  | MkCase a [(Fix PatternF, a)]
  | MkIfThenElse a a a
  deriving (Functor)

type ElmValue' = Fix ElmValueF

newtype T t a = T {unT :: a}
  deriving (Functor)

type ElmValue t = T t ElmValue'

instance IsString (ElmValue String) where
  fromString = T . Fix . MkString . fromString

data PatternF p
  = VarPat Text
  | StringPat Text
  | IntPat Int32
  | ConstructorPat Text [p]
  | Tuple2Pat p p
  | Tuple3Pat p p p
  | RecordPat [Text]
  deriving (Functor)

type Pattern t = Fix PatternF

fn1 :: ElmValue (a -> b) -> ElmValue a -> ElmValue b
fn1 (T f) (T x) = T $ Fix (MkApply f x)

fn2 :: ElmValue (a -> b -> c) -> ElmValue a -> ElmValue b -> ElmValue c
fn2 (T f) (T x) (T y) = T $ Fix (MkApply (Fix (MkApply f x)) y)

fn3 :: ElmValue (a -> b -> c -> d) -> ElmValue a -> ElmValue b -> ElmValue c -> ElmValue d
fn3 (T f) (T x) (T y) (T z) = T $ Fix (MkApply (Fix (MkApply (Fix (MkApply f x)) y)) z)

fn4 :: ElmValue (a -> b -> c -> d -> e) -> ElmValue a -> ElmValue b -> ElmValue c -> ElmValue d -> ElmValue e
fn4 (T f) (T x) (T y) (T z) (T a) = T $ Fix (MkApply (Fix (MkApply (Fix (MkApply (Fix (MkApply f x)) y)) z)) a)

fn5 :: ElmValue (a -> b -> c -> d -> e -> f) -> ElmValue a -> ElmValue b -> ElmValue c -> ElmValue d -> ElmValue e -> ElmValue f
fn5 (T fn) (T a) (T b) (T c) (T d) (T e) = T $ Fix (MkApply (Fix (MkApply (Fix (MkApply (Fix (MkApply (Fix (MkApply fn a)) b)) c)) d)) e)

fn6 :: ElmValue (a -> b -> c -> d -> e -> f -> g) -> ElmValue a -> ElmValue b -> ElmValue c -> ElmValue d -> ElmValue e -> ElmValue f -> ElmValue g
fn6 (T fn) (T a) (T b) (T c) (T d) (T e) (T f) = T $ Fix (MkApply (Fix (MkApply (Fix (MkApply (Fix (MkApply (Fix (MkApply (Fix (MkApply fn a)) b)) c)) d)) e)) f)

fn7 :: ElmValue (a -> b -> c -> d -> e -> f -> g -> h) -> ElmValue a -> ElmValue b -> ElmValue c -> ElmValue d -> ElmValue e -> ElmValue f -> ElmValue g -> ElmValue h
fn7 (T fn) (T a) (T b) (T c) (T d) (T e) (T f) (T g) = T $ Fix (MkApply (Fix (MkApply (Fix (MkApply (Fix (MkApply (Fix (MkApply (Fix (MkApply (Fix (MkApply fn a)) b)) c)) d)) e)) f)) g)

fn8 :: ElmValue (a -> b -> c -> d -> e -> f -> g -> h -> i) -> ElmValue a -> ElmValue b -> ElmValue c -> ElmValue d -> ElmValue e -> ElmValue f -> ElmValue g -> ElmValue h -> ElmValue i
fn8 (T fn) (T a) (T b) (T c) (T d) (T e) (T f) (T g) (T h) = T $ Fix (MkApply (Fix (MkApply (Fix (MkApply (Fix (MkApply (Fix (MkApply (Fix (MkApply (Fix (MkApply (Fix (MkApply fn a)) b)) c)) d)) e)) f)) g)) h)

fn9 :: ElmValue (a -> b -> c -> d -> e -> f -> g -> h -> i -> j) -> ElmValue a -> ElmValue b -> ElmValue c -> ElmValue d -> ElmValue e -> ElmValue f -> ElmValue g -> ElmValue h -> ElmValue i -> ElmValue j
fn9 (T fn) (T a) (T b) (T c) (T d) (T e) (T f) (T g) (T h) (T i) = T $ Fix (MkApply (Fix (MkApply (Fix (MkApply (Fix (MkApply (Fix (MkApply (Fix (MkApply (Fix (MkApply (Fix (MkApply (Fix (MkApply fn a)) b)) c)) d)) e)) f)) g)) h)) i)

p0 :: Text -> Pattern t
p0 n = Fix (VarPat n)

-- |
-- Combine two Elm values with a right-pizza operator.
(|>) :: ElmValue a -> ElmValue (a -> b) -> ElmValue b
(|>) left right = fn2 (var "|>") left right

infixl 1 |>

-- |
-- Combine two Elm values with a left-pizza operator.
(<|) :: ElmValue (a -> b) -> ElmValue a -> ElmValue b
(<|) left right = fn2 (var "<|") left right

infixr 0 <|

lambda :: (Pattern a, ElmValue b) -> ElmValue (a -> b)
lambda (pattern, value) = T . Fix $ MkLambda pattern (unT value)

mkCase :: ElmValue a -> [(Pattern a, ElmValue b)] -> ElmValue b
mkCase (T matched) branches = T . Fix $ MkCase matched ((fmap . fmap) unT branches)

matchVar :: Text -> (ElmValue a -> ElmValue b) -> (Pattern a, ElmValue b)
matchVar name withMatch = (Fix (VarPat name), withMatch (v name))

matchString :: Text -> Pattern String
matchString = Fix . StringPat

matchInt :: Int32 -> Pattern Int32
matchInt = Fix . IntPat

matchTuple2 :: Text -> Text -> (ElmValue a -> ElmValue b -> ElmValue c) -> (Pattern (a, b), ElmValue c)
matchTuple2 name1 name2 withMatch =
  ( Fix (Tuple2Pat (Fix (VarPat name1)) (Fix (VarPat name2))),
    withMatch (v name1) (v name2)
  )

matchTuple3 ::
  Text ->
  Text ->
  Text ->
  (ElmValue a -> ElmValue b -> ElmValue c -> ElmValue d) ->
  (Pattern (a, b, c), ElmValue d)
matchTuple3 name1 name2 name3 withMatch =
  ( Fix (Tuple3Pat (Fix (VarPat name1)) (Fix (VarPat name2)) (Fix (VarPat name3))),
    withMatch (v name1) (v name2) (v name3)
  )

matchRecordN ::
  [Text] ->
  (Variable a -> ElmValue b) ->
  ([(Text, ElmValue b)] -> ElmValue c) ->
  (Pattern x, ElmValue c)
matchRecordN fields perField combine =
  ( Fix (RecordPat fields),
    combine . zip fields $ perField . Variable <$> fields
  )

matchCtor1Pattern ::
  Variable (y -> x) ->
  (Pattern x, ElmValue c) ->
  (Pattern y, ElmValue c)
matchCtor1Pattern (Variable ctor) (innerPattern, body) =
  (Fix (ConstructorPat ctor [innerPattern]), body)

newtype Variable t = Variable Text deriving (IsString)

varName :: Variable t -> Text
varName (Variable t) = t

fromVarName :: Text -> Variable t
fromVarName = Variable

matchCtor0 ::
  Variable a ->
  ElmValue c ->
  (Pattern b, ElmValue c)
matchCtor0 (Variable ctor) val =
  ( Fix (ConstructorPat ctor []),
    val
  )

matchCtor1 ::
  Variable (a -> b) ->
  Text ->
  (ElmValue a -> ElmValue c) ->
  (Pattern b, ElmValue c)
matchCtor1 (Variable ctor) param f =
  ( Fix (ConstructorPat ctor [Fix (VarPat param)]),
    f (v param)
  )

matchCtorN ::
  Variable x ->
  [Text] ->
  (Variable a -> ElmValue b) ->
  ([ElmValue b] -> ElmValue c) ->
  (Pattern y, ElmValue c)
matchCtorN (Variable ctor) params perParam combine =
  ( Fix (ConstructorPat ctor (Fix . VarPat <$> params)),
    combine $ perParam . Variable <$> params
  )

anyType :: ElmValue a -> ElmValue b
anyType (T x) = T x

unit :: ElmValue ()
unit = T $ Fix MkUnit

tuple :: ElmValue a -> ElmValue b -> ElmValue (a, b)
tuple (T x) (T y) = T $ Fix (MkTuple2 x y)

tuple3 :: ElmValue a -> ElmValue b -> ElmValue c -> ElmValue (a, b, c)
tuple3 (T x) (T y) (T z) = T $ Fix (MkTuple3 x y z)

data Record = Record [(Text, Fix ElmValueF)]

addField :: Text -> ElmValue a -> Record -> Record
addField name (T field) (Record fields) =
  Record ((name, field) : fields)

emptyRecord :: Record
emptyRecord = Record []

mkRecord :: Record -> ElmValue a
mkRecord (Record fields) =
  T (Fix (MkRecord fields))

ifThenElse :: ElmValue Bool -> ElmValue a -> ElmValue a -> ElmValue a
ifThenElse (T cond) (T if_) (T else_) = T (Fix (MkIfThenElse cond if_ else_))

list :: [ElmValue a] -> ElmValue [a]
list = T . Fix . MkList . fmap unT

string :: Text -> ElmValue String
string = T . Fix . MkString

int :: Int32 -> ElmValue Int
int = T . Fix . MkInt

v :: Text -> ElmValue a
v name = T $ Fix $ MkVar name

var :: Variable a -> ElmValue a
var (Variable s) = T $ Fix (MkVar s)

data ElmFunction where
  ElmFunction ::
    { fnName :: Text,
      fnType :: ElmType,
      fnImplementation :: ElmValue a
    } ->
    ElmFunction

printFunction :: ElmFunction -> Doc
printFunction ElmFunction {fnName, fnType, fnImplementation} =
  PP.vsep
    [ hangCollapse $ PP.pretty fnName <+> ":" <++> printType fnType,
      PP.nest elmIndent $ PP.pretty fnName <+> go (unT fnImplementation)
    ]
  where
    go =
      \case
        Fix (MkLambda pattern rest) ->
          printPattern True pattern <+> go rest
        x -> "=" <> PP.line <> printValue' x

printValue' :: Fix ElmValueF -> Doc
printValue' =
  histo $ \case
    MkUnit -> fromString "()"
    MkBool bool -> PP.pretty $ show bool
    MkInt int' -> PP.pretty $ show int'
    MkFloat double -> PP.pretty $ show double
    MkString text -> PP.dquotes (PP.pretty text)
    MkList items -> encloseSep' PP.lbracket PP.rbracket PP.comma (extract <$> items)
    MKMaybe a -> maybe (fromString "Nothing") ((fromString "Just" <+>) . extractParens) a
    MkTuple2 x y -> encloseSep' PP.lparen PP.rparen PP.comma [extract x, extract y]
    MkTuple3 x y z -> encloseSep' PP.lparen PP.rparen PP.comma [extract x, extract y, extract z]
    MkRecord fields ->
      encloseSep' PP.lbrace PP.rbrace PP.comma (printField . fmap extract <$> fields)
      where
        printField :: (Text, Doc) -> Doc
        printField (name, value) =
          hangCollapse $ PP.pretty name <+> fromString "=" <++> value
    MkLambda pattern1 body1 -> nextArg [pattern1] body1
      where
        nextArg :: [Pattern a] -> Cofree ElmValueF Doc -> Doc
        nextArg patterns (body :< peek) =
          case peek of
            MkLambda nextPattern nextBody ->
              nextArg (nextPattern : patterns) nextBody
            _ ->
              hangCollapse $
                fromString "\\"
                  <> PP.hsep (printPattern True <$> patterns)
                  <+> fromString "->"
                  <++> body
    MkApply rest1 x1 ->
      nextArg (mempty :< MkApply rest1 x1) []
      where
        nextArg next args =
          case next of
            -- Pattern match on functions acting on two arguments, because these
            -- could be operators that require custom formatting logic.
            (_ :< MkApply (_ :< MkApply (_ :< MkVar fn) arg1) arg2) ->
              case fn of
                "|>" -> extract arg1 <> PP.hardline <> PP.indent elmIndent (PP.pretty fn <+> extract arg2)
                "<|" -> hangCollapse $ extract arg1 <+> PP.pretty fn <++> extract arg2
                _ -> hangCollapse $ PP.vsep (PP.pretty fn : extractParens arg1 : extractParens arg2 : args)
            -- Recursively match on function application, adding one argument at
            -- a time.
            (_ :< MkApply rest2 x2) ->
              nextArg rest2 (extractParens x2 : args)
            -- When no more function applications are found, the value we find
            -- at the root is the function name itself.
            (f :< more) ->
              hangCollapse $ PP.vsep (PP.nest (- elmIndent) (extractParens (f :< more)) : args)
    MkVar name -> PP.pretty name
    MkCase matched branches ->
      fromString "case"
        <+> extract matched
        <+> fromString "of"
        <++> PP.indent
          elmIndent
          ( PP.vcat $
              PP.punctuate
                (PP.line)
                (uncurry printBranch . fmap extract <$> branches)
          )
      where
        printBranch :: Pattern t -> Doc -> Doc
        printBranch match body =
          printPattern False match <+> fromString "->" <++> PP.indent elmIndent body
    MkIfThenElse cond if_ else_ ->
      PP.column $ \column ->
        "if"
          <+> extract cond
          <+> "then"
            <> PP.hardline
            <> PP.indent elmIndent (extract if_)
            <> PP.hardline
            <> PP.hardline
            <> setIndent column "else"
            <> PP.hardline
            <> PP.indent elmIndent (extract else_)

extractParens :: Cofree ElmValueF Doc -> Doc
extractParens (val :< prev) =
  parens (appearance prev) val

appearance :: ElmValueF a -> TypeAppearance
appearance =
  \case
    MkUnit -> SingleWord
    MkBool _ -> SingleWord
    MkInt _ -> SingleWord
    MkFloat _ -> SingleWord
    MkString _ -> SingleWord
    MkList _ -> SingleWord
    MKMaybe a -> maybe SingleWord (\_ -> MultipleWord) a
    MkTuple2 _ _ -> SingleWord
    MkTuple3 _ _ _ -> SingleWord
    MkRecord _ -> SingleWord
    MkLambda _ _ -> MultipleWord
    MkApply _ _ -> MultipleWord
    MkVar _ -> SingleWord
    MkCase _ _ -> MultipleWord
    MkIfThenElse _ _ _ -> MultipleWord

printPattern :: Bool -> Pattern t -> Doc
printPattern needsConstructorParens =
  cata $ \case
    VarPat name -> PP.pretty name
    StringPat str -> PP.dquotes (PP.pretty str)
    IntPat n -> (PP.pretty n)
    ConstructorPat ctor vars ->
      case vars of
        [] -> PP.pretty ctor
        _ ->
          PP.group
            $ (if needsConstructorParens then PP.parens else id)
            $ PP.sep
            $ PP.pretty ctor : vars
    Tuple2Pat x y ->
      encloseSep' PP.lparen PP.rparen PP.comma [x, y]
    Tuple3Pat x y z ->
      encloseSep' PP.lparen PP.rparen PP.comma [x, y, z]
    RecordPat fields ->
      encloseSep' PP.lbrace PP.rbrace PP.comma (PP.pretty <$> fields)

-- * Library

_Just :: Variable (a -> Maybe a)
_Just = "Just"

_Nothing :: Variable (Maybe a)
_Nothing = "Nothing"

data Result e a

_Err :: Variable (e -> Result e a)
_Err = "Err"

_Ok :: Variable (a -> Result e a)
_Ok = "Ok"

_always :: Variable (a -> b -> a)
_always = "always"

_identity :: Variable (a -> a)
_identity = "identity"

data Never

_never :: Variable (Never -> a)
_never = "never"

_String_fromInt :: Variable (Int -> String)
_String_fromInt = "String.fromInt"

_String_fromFloat :: Variable (Float -> String)
_String_fromFloat = "String.fromFloat"

_String_join :: Variable (String -> [String] -> String)
_String_join = "String.join"

_String_concat :: Variable ([String] -> String)
_String_concat = "String.concat"

_Tuple_pair :: Variable (a -> b -> (a, b))
_Tuple_pair = "Tuple.pair"

_List_map :: Variable ((a -> b) -> [a] -> [b])
_List_map = "List.map"

_List_concat :: Variable ([[a]] -> [a])
_List_concat = "List.concat"

_List_intersperse :: Variable (a -> [a] -> [a])
_List_intersperse = "List.intersperse"

data Value

_Json_Encode_encode :: Variable (Int -> Value -> String)
_Json_Encode_encode = "Json.Encode.encode"

_Json_Encode_list :: Variable ((a -> Value) -> [a] -> Value)
_Json_Encode_list = "Json.Encode.list"

_Json_Encode_bool :: Variable (Bool -> Value)
_Json_Encode_bool = "Json.Encode.bool"

_Json_Encode_int :: Variable (Int -> Value)
_Json_Encode_int = "Json.Encode.int"

_Json_Encode_float :: Variable (Float -> Value)
_Json_Encode_float = "Json.Encode.float"

_Json_Encode_string :: Variable (String -> Value)
_Json_Encode_string = "Json.Encode.string"

_Json_Encode_object :: Variable ([(String, Value)] -> Value)
_Json_Encode_object = "Json.Encode.object"

_Json_Encode_null :: Variable Value
_Json_Encode_null = "Json.Encode.null"

data Decoder a

_Json_Decode_string :: Variable (Decoder String)
_Json_Decode_string = "Json.Decode.string"

_Json_Decode_bool :: Variable (Decoder Bool)
_Json_Decode_bool = "Json.Decode.bool"

_Json_Decode_int :: Variable (Decoder Int)
_Json_Decode_int = "Json.Decode.int"

_Json_Decode_float :: Variable (Decoder Float)
_Json_Decode_float = "Json.Decode.float"

_Json_Decode_nullable :: Variable (Decoder a -> Decoder (Maybe a))
_Json_Decode_nullable = "Json.Decode.nullable"

_Json_Decode_list :: Variable (Decoder a -> Decoder [a])
_Json_Decode_list = "Json.Decode.list"

data Array a

_Json_Decode_array :: Variable (Decoder a -> Decoder (Array a))
_Json_Decode_array = "Json.Decode.array"

data Dict k v

_Json_Decode_dict :: Variable (Decoder a -> Decoder (Dict String a))
_Json_Decode_dict = "Json.Decode.dict"

_Json_Decode_keyValuePairs :: Variable (Decoder a -> Decoder [(String, a)])
_Json_Decode_keyValuePairs = "Json.Decode.keyValuePairs"

_Json_Decode_field :: Variable (String -> Decoder a -> Decoder a)
_Json_Decode_field = "Json.Decode.field"

_Json_Decode_at :: Variable ([String] -> Decoder a -> Decoder a)
_Json_Decode_at = "Json.Decode.at"

_Json_Decode_index :: Variable (Int -> Decoder a -> Decoder a)
_Json_Decode_index = "Json.Decode.index"

_Json_Decode_maybe :: Variable (Decoder a -> Decoder (Maybe a))
_Json_Decode_maybe = "Json.Decode.maybe"

_Json_Decode_oneOf :: Variable ([Decoder a] -> Decoder a)
_Json_Decode_oneOf = "Json.Decode.oneOf"

data Error

_Json_Decode_decodeString :: Variable (Decoder a -> String -> Result Error a)
_Json_Decode_decodeString = "Json.Decode.decodeString"

_Json_Decode_decodeValue :: Variable (Decoder a -> Value -> Result Error a)
_Json_Decode_decodeValue = "Json.Decode.decodeValue"

_Json_Decode_map :: Variable ((a -> value) -> Decoder a -> Decoder value)
_Json_Decode_map = "Json.Decode.map"

_Json_Decode_map2 :: Variable ((a -> b -> value) -> Decoder a -> Decoder b -> Decoder value)
_Json_Decode_map2 = "Json.Decode.map2"

_Json_Decode_map3 :: Variable ((a -> b -> c -> value) -> Decoder a -> Decoder b -> Decoder c -> Decoder value)
_Json_Decode_map3 = "Json.Decode.map3"

_Json_Decode_map4 :: Variable ((a -> b -> c -> d -> value) -> Decoder a -> Decoder b -> Decoder c -> Decoder d -> Decoder value)
_Json_Decode_map4 = "Json.Decode.map4"

_Json_Decode_map5 :: Variable ((a -> b -> c -> d -> e -> value) -> Decoder a -> Decoder b -> Decoder c -> Decoder d -> Decoder e -> Decoder value)
_Json_Decode_map5 = "Json.Decode.map5"

_Json_Decode_map6 :: Variable ((a -> b -> c -> d -> e -> f -> value) -> Decoder a -> Decoder b -> Decoder c -> Decoder d -> Decoder e -> Decoder f -> Decoder value)
_Json_Decode_map6 = "Json.Decode.map6"

_Json_Decode_map7 :: Variable ((a -> b -> c -> d -> e -> f -> g -> value) -> Decoder a -> Decoder b -> Decoder c -> Decoder d -> Decoder e -> Decoder f -> Decoder g -> Decoder value)
_Json_Decode_map7 = "Json.Decode.map7"

_Json_Decode_map8 :: Variable ((a -> b -> c -> d -> e -> f -> g -> h -> value) -> Decoder a -> Decoder b -> Decoder c -> Decoder d -> Decoder e -> Decoder f -> Decoder g -> Decoder h -> Decoder value)
_Json_Decode_map8 = "Json.Decode.map8"

_Json_Decode_lazy :: Variable ((() -> Decoder a) -> Decoder a)
_Json_Decode_lazy = "Json.Decode.lazy"

_Json_Decode_value :: Variable (Decoder Value)
_Json_Decode_value = "Json.Decode.value"

_Json_Decode_null :: Variable (a -> Decoder a)
_Json_Decode_null = "Json.Decode.null"

_Json_Decode_succeed :: Variable (a -> Decoder a)
_Json_Decode_succeed = "Json.Decode.succeed"

_Json_Decode_fail :: Variable (String -> Decoder a)
_Json_Decode_fail = "Json.Decode.fail"

_Json_Decode_andThen :: Variable ((a -> Decoder b) -> Decoder a -> Decoder b)
_Json_Decode_andThen = "Json.Decode.andThen"

data Request

data Cmd msg

_Http_request :: Variable (Request -> Cmd msg)
_Http_request = "Http.request"

data Header

_Http_header :: Variable (String -> String -> Header)
_Http_header = "Http.header"

data Body

_Http_emptyBody :: Variable Body
_Http_emptyBody = "Http.emptyBody"

_Http_jsonBody :: Variable (Value -> Body)
_Http_jsonBody = "Http.jsonBody"

data Expect msg

_Http_expectWhatever :: Variable ((Result Error () -> msg) -> Expect msg)
_Http_expectWhatever = "Http.expectWhatever"

_Http_expectJson :: Variable ((Result Error a -> msg) -> Decoder a -> Expect msg)
_Http_expectJson = "Http.expectJson"

data QueryParameter

_Url_Builder_absolute :: Variable ([String] -> [QueryParameter] -> String)
_Url_Builder_absolute = "Url.Builder.absolute"

_Url_Builder_string :: Variable (String -> String -> QueryParameter)
_Url_Builder_string = "Url.Builder.string"

_Url_Builder_int :: Variable (String -> Int -> QueryParameter)
_Url_Builder_int = "Url.Builder.int"
