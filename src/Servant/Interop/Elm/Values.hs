{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Servant.Interop.Elm.Values
  ( ElmType
  , ElmValue
  , ElmValueF(..)
  , Pattern
  , PatternF(..)
  , Variable
  , varName
  , printValue
  , anyType
  , unit
  , lambda
  , fn1
  , fn2
  , (<|)
  , (|>)
  , mkCase
  , matchVar
  , matchCtor0
  , matchCtor1
  , p0
  , list
  , string
  , var
  , tuple

  -- * Library functions
  , _Just
  , _Nothing
  , _Err
  , _Ok
  , _always
  , _identity
  , _never
  , _List_map
  , _Json_Encode_list
  , _Json_Encode_bool
  , _Json_Encode_int
  , _Json_Encode_float
  , _Json_Encode_string
  , _Json_Encode_object
  , _Json_Encode_null

  -- * Phantom types
  , Value
  , Result
  ) where


import Data.List.NonEmpty (NonEmpty)
import Data.Functor.Foldable (Fix(Fix), cata)
import Data.Int (Int32)
import Data.String (IsString(fromString))
import qualified Wire
import Data.Text (Text)
import Servant.Interop.Elm.Print
import Servant.Interop.Elm.Types (ElmType)
import Text.PrettyPrint.Leijen.Text ((<+>))
import qualified Data.Text as Text
import qualified Text.PrettyPrint.Leijen.Text as PP

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
  deriving (Functor)

type ElmValue' = Fix ElmValueF

newtype T t a = T { unT :: a }
  deriving (Functor)

type ElmValue t = T t ElmValue' 

instance IsString (ElmValue String) where
  fromString = T . Fix . MkString . fromString

data PatternF p
  = VarPat Text
  | ConstructorPat Text [p]
  | Tuple2Pat p p
  | RecordPat [Text]
  deriving (Functor)

type Pattern t = Fix PatternF

fn1 :: ElmValue (a -> b) -> ElmValue a -> ElmValue b
fn1 (T f) (T x) = T $ Fix (MkApply f x)

fn2 :: ElmValue (a -> b -> c) -> ElmValue a -> ElmValue b -> ElmValue c
fn2 (T f) (T x) (T y) = T $ Fix (MkApply (Fix (MkApply f x)) y)

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

lambda :: Text -> (ElmValue a -> ElmValue b) -> ElmValue (a -> b)
lambda var' body = T . Fix $ MkLambda (p0 var') (unT . body $ v var')

mkCase :: ElmValue a -> [(Pattern a, ElmValue b)] -> ElmValue b
mkCase (T matched) branches = T . Fix $ MkCase matched ((fmap . fmap) unT branches)

matchVar :: Text -> (ElmValue a -> ElmValue b) -> (Pattern a, ElmValue b)
matchVar name withMatch = (Fix (VarPat name), withMatch (v name))

newtype Variable t = Variable Text deriving (IsString)

varName :: Variable t -> Text
varName (Variable t) = t

matchCtor0
  :: Variable a
  -> ElmValue c
  -> (Pattern b, ElmValue c)
matchCtor0 (Variable ctor) val =
    ( Fix (ConstructorPat ctor [])
    , val
    )

matchCtor1
  :: Variable (a -> b)
  -> Text
  -> (ElmValue a -> ElmValue c)
  -> (Pattern b, ElmValue c)
matchCtor1 (Variable ctor) var' f =
    ( Fix (ConstructorPat ctor [Fix (VarPat var')])
    , f (v var')
    )
  
_caseFor
  :: NonEmpty (Wire.ConstructorName, [ElmType])
  -> (forall a. ElmValue a -> ElmValue b)
  -> ([(Text, ElmValue b)] -> ElmValue c)
  -> ElmValue c 
_caseFor _ctors _forParam _concatParams = undefined

anyType :: ElmValue a -> ElmValue b
anyType (T x) = T x

unit :: ElmValue ()
unit = T $ Fix MkUnit

tuple :: ElmValue a -> ElmValue b -> ElmValue (a, b)
tuple (T x) (T y) = T $ Fix (MkTuple2 x y)

list :: [ElmValue a] -> ElmValue [a]
list = T . Fix . MkList . fmap unT

string :: Text -> ElmValue String
string = T . Fix . MkString

v :: Text -> ElmValue a
v name = T $ Fix $ MkVar name

var :: Variable a -> ElmValue a
var (Variable s) = T $ Fix (MkVar s)

printValue :: ElmValue t -> PP.Doc
printValue = printValue' . unT

printValue' :: Fix ElmValueF -> PP.Doc
printValue' =
  cata $ \case
    MkUnit -> fromString "()"
    MkBool bool -> PP.textStrict . Text.pack $ show bool
    MkInt int -> PP.textStrict . Text.pack $ show int
    MkFloat double -> PP.textStrict . Text.pack $ show double
    MkString text -> PP.textStrict text
    MkList items -> encloseSep' PP.lbracket PP.rbracket PP.comma items
    MKMaybe a -> maybe (fromString "Nothing") (fromString "Maybe" <+>) a
    MkTuple2 x y -> encloseSep' PP.lparen PP.rparen PP.comma [x, y]
    MkTuple3 x y z -> encloseSep' PP.lparen PP.rparen PP.comma [x, y, z]
    MkRecord fields ->
      encloseSep' PP.lbrace PP.rbrace PP.comma (printField <$> fields)
      where printField :: (Text, PP.Doc) -> PP.Doc
            printField (name, value) =
              PP.textStrict name <+> fromString "=" <+> value
    MkLambda pattern body ->
      fromString "\\" <+> printPattern pattern <+> fromString "->" <+> body
    MkApply f x -> f <+> PP.lparen <+> x <+> PP.rparen
    MkVar name -> PP.textStrict name
    MkCase matched branches ->
      fromString "case" <+>
      matched <+> fromString "of" <> PP.vsep (uncurry printBranch <$> branches)
      where printBranch :: Pattern t -> PP.Doc -> PP.Doc
            printBranch match body =
              printPattern match <+> fromString "->" <+> body

printPattern :: Pattern t -> PP.Doc
printPattern =
  cata $ \case
    VarPat name -> PP.textStrict name
    ConstructorPat ctor vars -> 
      PP.sep $ (PP.textStrict ctor) : vars
    Tuple2Pat x y ->
       encloseSep' PP.lparen PP.rparen PP.comma [x, y]
    RecordPat fields ->
      encloseSep' PP.lbrace PP.rbrace PP.comma (PP.textStrict <$> fields)

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

_List_map :: Variable ((a -> b) -> [a] -> [b])
_List_map = "List.map"

data Value

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
