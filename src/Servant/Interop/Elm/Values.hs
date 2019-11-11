{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Servant.Interop.Elm.Values
  ( printValue
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

class IsElmValue a where
  isElmValue :: a -> ElmValue

instance IsElmValue () where
  isElmValue () = Fix MkUnit

instance IsElmValue String where
  isElmValue name = Fix $ (MkFnCall (VariableName (Text.pack name))) []

instance IsElmValue VariableName where
  isElmValue name = Fix $ MkFnCall name []

instance IsElmValue ElmValue where
  isElmValue = id

class IsElmArg t where
  isElmArg :: VariableName -> [ElmValue] -> t

instance IsElmArg ElmValue where
  isElmArg name args = Fix $ MkFnCall name args

instance (IsElmValue a, IsElmArg t) => IsElmArg (a -> t) where
  isElmArg name args arg = isElmArg name (isElmValue arg : args)

call :: (IsVarName s, IsElmArg args) => s -> args
call name = isElmArg (isVarName name) []

mkString :: Text -> ElmValue
mkString = Fix . MkString

mkList :: [ElmValue] -> ElmValue
mkList = Fix . MkList

mkTuple2 :: ElmValue -> ElmValue -> ElmValue
mkTuple2 a b = Fix $ MkTuple2 a b

lambda :: String -> (VariableName -> ElmValue) -> ElmValue
lambda var body = Fix $ MkLambda [pattern var] (body (isVarName var))

mkCase :: ElmValue -> [(Pattern, ElmValue)] -> ElmValue
mkCase matched branches = Fix $ MkCase matched branches

newtype VariableName = VariableName
  { unVariableName :: Text
  } deriving (IsString)

-- | A pattern to match on, in case statements or function arguments.
data Pattern =
  Match VariableName
        [Pattern]

pattern :: (IsVarName s, IsPatternArg args) => s -> args
pattern name = isPatternArg (isVarName name) []

class IsPatternArg t where
  isPatternArg :: VariableName -> [Pattern] -> t

instance IsPatternArg Pattern where
  isPatternArg name args = Match name args

instance (IsPattern a, IsPatternArg t) => IsPatternArg (a -> t) where
  isPatternArg name args arg = isPatternArg name (isPattern arg : args)

class IsPattern a where
  isPattern :: a -> Pattern

instance IsPattern Pattern where
  isPattern = id

instance IsPattern String where
  isPattern name = Match (isVarName name) []

class IsVarName a where
  isVarName :: a -> VariableName

instance IsVarName VariableName where
  isVarName = id

instance IsVarName String where
  isVarName name = VariableName (Text.pack name)

type ElmValue = Fix ElmValueF

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

_elmEncoder :: ElmType -> ElmValue
_elmEncoder =
  cata $ \case
    Unit -> call "Basics.always" ()
    Never -> call "Basics.never"
    Bool -> call "Json.Encode.bool"
    Int -> call "Json.Encode.int"
    Float -> call "Json.Encode.float"
    String -> call "Json.Encode.string"
    List a ->
      lambda "list" $ \list ->
        call "List.map" a list |> call "Json.Encode.list" a
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
    mkCase (call x) (uncurry constructorEncoder . first isVarName <$> ctors)

(|>) :: ElmValue -> ElmValue -> ElmValue
(|>) left right = call "Basics.|>" left right

infixl 1 |>

(<|) :: ElmValue -> ElmValue -> ElmValue
(<|) left right = call "Basics.<|" left right

infixr 0 <|

constructorEncoder :: VariableName -> [ElmValue] -> (Pattern, ElmValue)
constructorEncoder name paramEncoders =
  let vars =
        take (length paramEncoders) $
        (isVarName . ("x" <>) . show) <$> ([1 ..] :: [Int])
   in ( Match name (pattern <$> vars)
      , recordEncoder
          [ ("ctor", call "Json.Encode.string" name)
          , ( "value"
            , mkList $
              zipWith
                (\param encoder -> encoder <| isElmValue param)
                vars
                paramEncoders)
          ])

recordEncoder :: [(String, ElmValue)] -> ElmValue
recordEncoder fields =
  call "Json.Encode.object" (mkList $ uncurry fieldEncoder <$> fields)
  where
    fieldEncoder :: String -> ElmValue -> ElmValue
    fieldEncoder name = mkTuple2 (mkString (Text.pack name))
