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
  , fromVarName
  , printFunction
  , anyType
  , unit
  , lambda
  , fn1
  , fn2
  , (<|)
  , (|>)
  , mkCase
  , matchVar
  , matchTuple2
  , matchTuple3
  , matchRecordN
  , matchCtorRecord
  , matchCtor0
  , matchCtor1
  , matchCtorN
  , p0
  , list
  , string
  , var
  , v
  , tuple

  -- * Library functions
  -- ** Basics
  , _Just
  , _Nothing
  , _Err
  , _Ok
  , _always
  , _identity
  , _never

  -- ** List
  , _List_map

  -- ** Json.Encode
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


import Data.Functor.Foldable (Fix(Fix), cata, histo)
import Data.Int (Int32)
import Data.String (IsString(fromString))
import Data.Text (Text)
import Servant.Interop.Elm.Print
import Servant.Interop.Elm.Types (ElmType, printType)
import Text.PrettyPrint.Leijen.Text ((<+>))
import Control.Comonad (extract)
import Control.Comonad.Cofree (Cofree((:<)))
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
  | Tuple3Pat p p p
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

lambda :: (Pattern a, ElmValue b) -> ElmValue (a -> b)
lambda (pattern, value) = T . Fix $ MkLambda pattern (unT value)

mkCase :: ElmValue a -> [(Pattern a, ElmValue b)] -> ElmValue b
mkCase (T matched) branches = T . Fix $ MkCase matched ((fmap . fmap) unT branches)

matchVar :: Text -> (ElmValue a -> ElmValue b) -> (Pattern a, ElmValue b)
matchVar name withMatch = (Fix (VarPat name), withMatch (v name))

matchTuple2 :: Text -> Text -> (ElmValue a -> ElmValue b -> ElmValue c) -> (Pattern (a, b), ElmValue c)
matchTuple2 name1 name2 withMatch =
  (Fix (Tuple2Pat (Fix (VarPat name1)) (Fix (VarPat name2)))
  , withMatch (v name1) (v name2)
  )

matchTuple3
  :: Text
  -> Text
  -> Text
  -> (ElmValue a -> ElmValue b -> ElmValue c -> ElmValue d)
  -> (Pattern (a, b, c), ElmValue d)
matchTuple3 name1 name2 name3 withMatch =
  (Fix (Tuple3Pat (Fix (VarPat name1)) (Fix (VarPat name2)) (Fix (VarPat name3)))
  , withMatch (v name1) (v name2) (v name3)
  )

matchRecordN
  :: [Text]
  -> (Variable a -> ElmValue b)
  -> ([ElmValue b] -> ElmValue c)
  -> (Pattern x, ElmValue c)
matchRecordN fields perField combine =
  ( Fix (RecordPat fields)
  , combine $ perField . Variable <$> fields
  )

matchCtorRecord
  :: Variable (y -> x)
  -> [Text]
  -> (Variable a -> ElmValue b)
  -> ([ElmValue b] -> ElmValue c)
  -> (Pattern x, ElmValue c)
matchCtorRecord (Variable ctor) fields perField combine =
  (Fix (ConstructorPat ctor [recPattern]), body)
    where (recPattern, body) = matchRecordN fields perField combine

newtype Variable t = Variable Text deriving (IsString)

varName :: Variable t -> Text
varName (Variable t) = t

fromVarName :: Text -> Variable t
fromVarName = Variable

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
matchCtor1 (Variable ctor) param f =
    ( Fix (ConstructorPat ctor [Fix (VarPat param)])
    , f (v param)
    )
 
matchCtorN
  :: Variable x
  -> [Text]
  -> (Variable a -> ElmValue b)
  -> ([ElmValue b] -> ElmValue c)
  -> (Pattern y, ElmValue c)
matchCtorN (Variable ctor) params perParam combine =
  ( Fix (ConstructorPat ctor (Fix . VarPat <$> params))
  , combine $ perParam . Variable <$> params
  )

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

printFunction :: Text -> ElmType -> ElmValue t -> PP.Doc
printFunction name fnType (T val) =
  PP.vsep
    [ PP.nest elmIndent $ PP.textStrict name <+> "::" <+> printType fnType
    , PP.nest elmIndent $ PP.textStrict name <+> go val
    ]
  where
    go =
      \case 
        Fix (MkLambda pattern rest) ->
          printPattern pattern <+> go rest
        x -> "=" <> PP.line <> printValue' x
        
printValue' :: Fix ElmValueF -> PP.Doc
printValue' =
  histo $ \case 
    MkUnit -> fromString "()"
    MkBool bool -> PP.textStrict . Text.pack $ show bool
    MkInt int -> PP.textStrict . Text.pack $ show int
    MkFloat double -> PP.textStrict . Text.pack $ show double
    MkString text -> PP.dquotes (PP.textStrict text)
    MkList items -> PP.group $ encloseSep' PP.lbracket PP.rbracket PP.comma (extract <$> items)
    MKMaybe a -> maybe (fromString "Nothing") ((fromString "Just" <+>) . extractParens) a
    MkTuple2 x y -> PP.group $ encloseSep' PP.lparen PP.rparen PP.comma [extract x, extract y]
    MkTuple3 x y z -> PP.group $ encloseSep' PP.lparen PP.rparen PP.comma [extract x, extract y, extract z]
    MkRecord fields ->
      encloseSep' PP.lbrace PP.rbrace PP.comma (printField . fmap extract <$> fields)
      where printField :: (Text, PP.Doc) -> PP.Doc
            printField (name, value) =
              PP.textStrict name <+> fromString "=" <+> value
    MkLambda pattern body ->
      hangCollapse $ fromString "\\" <> printPattern pattern <+> fromString "->" <++> extract body
    MkApply rest1 x1 ->
      hangCollapse $ nextArg rest1 (extractParens x1)
      where 
        nextArg (f :< more) memo =
          case more of
            MkApply rest2 x2 -> nextArg rest2 (extractParens x2 <++> memo)
            _ -> extractParens (f :< more) <++> memo
    MkVar name -> PP.textStrict name
    MkCase matched branches ->
      fromString "case" <+>
      extract matched <+>
      fromString "of" <++>
      PP.indent elmIndent (PP.vsep (uncurry printBranch . fmap extract <$> branches))
      where printBranch :: Pattern t -> PP.Doc -> PP.Doc
            printBranch match body =
              printPattern match <+> fromString "->" <++> PP.indent elmIndent body

extractParens :: Cofree ElmValueF PP.Doc -> PP.Doc
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

printPattern :: Pattern t -> PP.Doc
printPattern =
  cata $ \case
    VarPat name -> PP.textStrict name
    ConstructorPat ctor vars -> 
      PP.group $ PP.sep $ (PP.textStrict ctor) : vars
    Tuple2Pat x y ->
      PP.group $ encloseSep' PP.lparen PP.rparen PP.comma [x, y]
    Tuple3Pat x y z ->
      PP.group $ encloseSep' PP.lparen PP.rparen PP.comma [x, y, z]
    RecordPat fields ->
      PP.group $ encloseSep' PP.lbrace PP.rbrace PP.comma (PP.textStrict <$> fields)

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
