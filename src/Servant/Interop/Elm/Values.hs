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
  ( printValue
  , elmEncoder
  ) where

import Data.Functor.Foldable (Fix(Fix), cata)
import GHC.TypeLits (Symbol)
import Data.Int (Int32)
import Data.String (IsString(fromString))
import Data.Text (Text)
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
  | MkTuple2 a
             a
  | MkTuple3 a
             a
             a
  | MkRecord [(Text, a)]
  | MkLambda [Pattern]
             a
  | MkFnCall VariableName
             [a]
  | MkCase a
           [(Pattern, a)]
  deriving (Functor)

type ElmValue (t :: ElmType) = Fix ElmValueF

type ElmType = Fix (ElmTypeF' Symbol)

instance IsString (ElmValue ('Fix 'String)) where
  fromString = Fix . MkString . fromString

newtype VariableName = VariableName
  { unVariableName :: Text
  } deriving (IsString)

-- | A pattern to match on, in case statements or function arguments.
data Pattern =
  Match VariableName
        [Pattern]

fn1 :: ElmValue ('Fix ('Lambda a b)) -> ElmValue a -> ElmValue b
fn1 f x = f <| x

fn2 :: ElmValue ('Fix ('Lambda a ('Fix ('Lambda b c)))) -> ElmValue a -> ElmValue b -> ElmValue c
fn2 f x y = f <| x <| y

p0 :: VariableName -> Pattern
p0 n = Match n []

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

lambda :: VariableName -> (ElmValue a -> ElmValue b) -> ElmValue ('Fix ('Lambda a b))
lambda var body = Fix $ MkLambda [p0 var] (body (Fix (MkFnCall var [])))

mkCase :: ElmValue a -> [(Pattern, ElmValue b)] -> ElmValue b
mkCase matched branches = Fix $ MkCase matched branches

unit :: ElmValue ('Fix 'Unit)
unit = Fix MkUnit

tuple :: ElmValue a -> ElmValue b -> ElmValue ('Fix ('Tuple2 a b))
tuple x y = Fix (MkTuple2 x y)

l :: [ElmValue a] -> ElmValue ('Fix ('List a))
l = Fix . MkList

v :: VariableName -> ElmValue a
v name = Fix $ MkFnCall name []

printValue :: ElmValue a -> PP.Doc
printValue = printValue'

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
    MkLambda params body ->
      fromString "\\" <+>
      PP.sep (printPattern <$> params) <+> fromString "->" <+> body
    MkFnCall name args -> PP.sep (printVariableName name : args)
    MkCase matched branches ->
      fromString "case" <+>
      matched <+> fromString "of" <> PP.vsep (uncurry printBranch <$> branches)
      where printBranch :: Pattern -> PP.Doc -> PP.Doc
            printBranch match body =
              printPattern match <+> fromString "->" <+> body

printPattern :: Pattern -> PP.Doc
printPattern (Match ctor vars) =
  PP.sep $ (printVariableName ctor) : (printPattern <$> vars)

printVariableName :: VariableName -> PP.Doc
printVariableName = PP.textStrict . unVariableName

elmEncoder :: ElmType -> ElmValue a
elmEncoder =
  cata $ \case
    Unit -> fn1 "Basics.always" unit
    Never -> "Basics.never"
    Bool -> "Json.Encode.bool"
    Int -> "Json.Encode.int"
    Float -> "Json.Encode.float"
    String -> "Json.Encode.string"
    List a ->
      lambda "list" $ \list ->
        fn2 "List.map" a list |> fn1 "Json.Encode.list" a
    Maybe a -> customTypeEncoder [("Nothing", []), ("Just", [a])]
    Result err ok -> customTypeEncoder [("Err", [err]), ("Ok", [ok])]
    Tuple2 _a _b -> undefined
    Tuple3 _a _b _c -> undefined
    Record _fields -> undefined
    Lambda _i _o -> undefined
    Defined _name _ -> undefined

customTypeEncoder :: [(VariableName, [ElmValue x])] -> ElmValue y
customTypeEncoder ctors =
  lambda ("x") $ \x ->
    mkCase x (uncurry constructorEncoder <$> ctors)

constructorEncoder :: VariableName -> [ElmValue x] -> (Pattern, ElmValue y)
constructorEncoder name paramEncoders =
  let vars =
        take (length paramEncoders) $ (("x" <>) . show) <$> ([1 ..] :: [Int])
   in ( Match name (p0 . fromString <$> vars)
      , fn1
          ("Json.Encode.object")
          (l [ tuple ("ctor") (fn1 ("Json.Encode.string") (v name))
          , tuple ("value")
            ( l $
              zipWith
                (\param encoder -> encoder <| fromString param)
                vars
                paramEncoders)
          ]))
