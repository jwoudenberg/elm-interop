{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module Servant.Interop.Elm.Values
  ( printValue
  , elmEncoder
  ) where

import Data.Bifunctor (first)
import Data.Functor.Foldable (Fix(Fix), cata)
import Data.Int (Int32)
import Data.String (IsString(fromString))
import Data.Text (Text)
import Servant.Interop.Elm.Print
import Servant.Interop.Elm.Types
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

type ElmValue = Fix ElmValueF

newtype VariableName = VariableName
  { unVariableName :: Text
  } deriving (IsString)

-- | A pattern to match on, in case statements or function arguments.
data Pattern =
  Match VariableName
        [Pattern]

-- |
-- Create Elm variables or function calls.
--
--    function "variable"
--
--    function "List.map" (function "Basics.abs") -23
--
function :: (IsVarName s, IsElmArg args) => s -> args
function name = isElmArg (isVarName name) []

-- |
-- Create an Elm pattern.
--
--     pattern "User" "firstName" "lastName"
--
pattern :: (IsVarName s, IsPatternArg args) => s -> args
pattern name = isPatternArg (isVarName name) []

-- |
-- Combine two Elm values with a right-pizza operator.
(|>) :: ElmValue -> ElmValue -> ElmValue
(|>) left right = function "Basics.|>" left right

infixl 1 |>

-- |
-- Combine two Elm values with a left-pizza operator.
(<|) :: ElmValue -> ElmValue -> ElmValue
(<|) left right = function "Basics.<|" left right

infixr 0 <|

lambda :: String -> (VariableName -> ElmValue) -> ElmValue
lambda var body = Fix $ MkLambda [pattern var] (body (isVarName var))

mkCase :: ElmValue -> [(Pattern, ElmValue)] -> ElmValue
mkCase matched branches = Fix $ MkCase matched branches

-- |
-- Helper type class for interpreting Haskell values as Elm values. This allows
-- us to use regular Haskell values in building Elm ASTs.
class IsElmValue a where
  isElmValue :: a -> ElmValue

instance IsElmValue () where
  isElmValue () = Fix MkUnit

instance IsElmValue Data.Int.Int32 where
  isElmValue int = Fix (MkInt int)

instance {-# OVERLAPPING #-} IsElmValue String where
  isElmValue string = Fix (MkString (Text.pack string))

instance (IsElmValue a, IsElmValue b) => IsElmValue (a, b) where
  isElmValue (x, y) = Fix (MkTuple2 (isElmValue x) (isElmValue y))

instance IsElmValue a => IsElmValue [a] where
  isElmValue = Fix . MkList . fmap isElmValue

instance IsElmValue VariableName where
  isElmValue name = Fix $ MkFnCall name []

instance IsElmValue ElmValue where
  isElmValue = id

-- |
-- Helper class for recognizing Elm args. This supports the `function` helper,
-- and allows it to take a variadic amount of arguments.
class IsElmArg t where
  isElmArg :: VariableName -> [ElmValue] -> t

instance IsElmArg ElmValue where
  isElmArg name args = Fix $ MkFnCall name args

instance (IsElmValue a, IsElmArg t) => IsElmArg (a -> t) where
  isElmArg name args arg = isElmArg name (isElmValue arg : args)

-- |
-- Helper class for recognizing pattern args. This supports the `pattern`
-- helper, and allows it to take a variadic amount of arguments.
class IsPatternArg t where
  isPatternArg :: VariableName -> [Pattern] -> t

instance IsPatternArg Pattern where
  isPatternArg name args = Match name args

instance (IsPattern a, IsPatternArg t) => IsPatternArg (a -> t) where
  isPatternArg name args arg = isPatternArg name (isPattern arg : args)

-- |
-- Helper class for recognizing patterns. This allows the `pattern` helper to
-- take a mix of `Pattern` types and regular strings.
class IsPattern a where
  isPattern :: a -> Pattern

instance IsPattern Pattern where
  isPattern = id

instance IsPattern String where
  isPattern name = Match (isVarName name) []

-- |
-- Helper class for recognizing variable names. This allows us to write strings
-- in places instead of explicitly wrapping our variables in the `VariableName`
-- constructor.
class IsVarName a where
  isVarName :: a -> VariableName

instance IsVarName VariableName where
  isVarName = id

instance IsVarName String where
  isVarName name = VariableName (Text.pack name)

printValue :: ElmValue -> PP.Doc
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

elmEncoder :: ElmType -> ElmValue
elmEncoder =
  cata $ \case
    Unit -> function "Basics.always" ()
    Never -> function "Basics.never"
    Bool -> function "Json.Encode.bool"
    Int -> function "Json.Encode.int"
    Float -> function "Json.Encode.float"
    String -> function "Json.Encode.string"
    List a ->
      lambda "list" $ \list ->
        function "List.map" a list |> function "Json.Encode.list" a
    Maybe a -> customTypeEncoder [("Nothing", []), ("Just", [a])]
    Result err ok -> customTypeEncoder [("Err", [err]), ("Ok", [ok])]
    Tuple2 _a _b -> undefined
    Tuple3 _a _b _c -> undefined
    Record _fields -> undefined
    Lambda _i _o -> undefined
    Defined _name -> undefined

customTypeEncoder :: IsVarName s => [(s, [ElmValue])] -> ElmValue
customTypeEncoder ctors =
  lambda "x" $ \x ->
    mkCase (function x) (uncurry constructorEncoder . first isVarName <$> ctors)

constructorEncoder :: VariableName -> [ElmValue] -> (Pattern, ElmValue)
constructorEncoder name paramEncoders =
  let vars =
        take (length paramEncoders) $ (("x" <>) . show) <$> ([1 ..] :: [Int])
   in ( Match name (pattern <$> vars)
      , function
          "Json.Encode.object"
          [ ("ctor", function "Json.Encode.string" name)
          , ( "value"
            , isElmValue $
              zipWith
                (\param encoder -> encoder <| isElmValue param)
                vars
                paramEncoders)
          ])
