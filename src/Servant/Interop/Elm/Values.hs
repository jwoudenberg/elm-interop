{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
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
  , printValue
  , unit
  , lambda
  , fn1
  , fn2
  , (<|)
  , (|>)
  , mkCase
  , p0
  , l
  , v
  , tuple
  ) where


import Data.Functor.Foldable (Fix(Fix), cata)
import Data.Int (Int32)
import Data.String (IsString(fromString))
import Data.Text (Text)
import GHC.TypeLits (Symbol)
import Servant.Interop.Elm.Print
import Servant.Interop.Elm.Types (ElmTypeF'(..))
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

type ElmValue (t :: ElmType) = Fix ElmValueF

type ElmType = Fix (ElmTypeF' Symbol)

instance IsString (ElmValue ('Fix 'String)) where
  fromString = Fix . MkString . fromString

data PatternF p
  = VarPat Text
  | ConstructorPat Text [p]
  | Tuple2Pat p p
  | RecordPat [Text]
  deriving (Functor)

type Pattern (t :: ElmType) = Fix PatternF

fn1 :: ElmValue ('Fix ('Lambda a b)) -> ElmValue a -> ElmValue b
fn1 f x = Fix (MkApply f x)

fn2 :: ElmValue ('Fix ('Lambda a ('Fix ('Lambda b c)))) -> ElmValue a -> ElmValue b -> ElmValue c
fn2 f x y = Fix (MkApply (Fix (MkApply f x)) y)

p0 :: Text -> Pattern t
p0 n = Fix (VarPat n)

-- |
-- Combine two Elm values with a right-pizza operator.
(|>) :: ElmValue a -> ElmValue ('Fix ('Lambda a b)) -> ElmValue b
(|>) left right = fn2 "Basics.|>" left right

infixl 1 |>

-- |
-- Combine two Elm values with a left-pizza operator.
(<|) :: ElmValue ('Fix ('Lambda a b)) -> ElmValue a -> ElmValue b
(<|) left right = fn2 "Basics.<|" left right

infixr 0 <|

lambda :: Text -> (ElmValue a -> ElmValue b) -> ElmValue ('Fix ('Lambda a b))
lambda var body = Fix $ MkLambda (p0 var) (body (v var))

mkCase :: ElmValue a -> [(Pattern a, ElmValue b)] -> ElmValue b
mkCase matched branches = Fix $ MkCase matched branches

unit :: ElmValue ('Fix 'Unit)
unit = Fix MkUnit

tuple :: ElmValue a -> ElmValue b -> ElmValue ('Fix ('Tuple2 a b))
tuple x y = Fix (MkTuple2 x y)

l :: [ElmValue a] -> ElmValue ('Fix ('List a))
l = Fix . MkList

v :: Text -> ElmValue a
v name = Fix $ MkVar name

printValue :: ElmValue t -> PP.Doc
printValue =
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
