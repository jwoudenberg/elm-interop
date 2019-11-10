{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Servant.Interop.Elm.Values
  ( printValue
  ) where

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

instance IsElmValue ElmValue where
  isElmValue = id

class IsElmArg t where
  isElmArg :: VariableName -> [ElmValue] -> t

instance IsElmArg ElmValue where
  isElmArg name args = Fix $ MkFnCall name args

instance (IsElmValue a, IsElmArg t) => IsElmArg (a -> t) where
  isElmArg name args arg = isElmArg name (isElmValue arg : args)

fn :: IsElmArg args => String -> args
fn name = isElmArg (VariableName (Text.pack name)) []

mkString :: Text -> ElmValue
mkString = Fix . MkString

mkList :: [ElmValue] -> ElmValue
mkList = Fix . MkList

mkTuple2 :: ElmValue -> ElmValue -> ElmValue
mkTuple2 a b = Fix $ MkTuple2 a b

mkLambda :: [Pattern] -> ElmValue -> ElmValue
mkLambda args body = Fix $ MkLambda args body

mkCase :: ElmValue -> [(Pattern, ElmValue)] -> ElmValue
mkCase matched branches = Fix $ MkCase matched branches

mkVar :: VariableName -> ElmValue
mkVar name = Fix $ MkFnCall name []

newtype VariableName = VariableName
  { unVariableName :: Text
  } deriving (IsString)

-- | A pattern to match on, in case statements or function arguments.
data Pattern =
  Match VariableName
        [Pattern]

instance IsString Pattern where
  fromString = flip Match [] . VariableName . Text.pack

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
            printBranch pattern body =
              printPattern pattern <+> fromString "->" <+> body

printPattern :: Pattern -> PP.Doc
printPattern (Match ctor vars) =
  PP.sep $ (printVariableName ctor) : (printPattern <$> vars)

printVariableName :: VariableName -> PP.Doc
printVariableName = PP.textStrict . unVariableName

_elmEncoder :: ElmType -> ElmValue
_elmEncoder =
  cata $ \case
    Unit -> fn "Basics.always" ()
    Never -> fn "Basics.never"
    Bool -> fn "Json.Encode.bool"
    Int -> fn "Json.Encode.int"
    Float -> fn "Json.Encode.float"
    String -> fn "Json.Encode.string"
    List a ->
      mkLambda [Match (VariableName (Text.pack "list")) []] $
      fn "List.map" a "list" `rightPizza` fn "Json.Encode.list" a
    Maybe a ->
      customTypeEncoder
        [ (VariableName (Text.pack "Nothing"), [])
        , (VariableName (Text.pack "Just"), [a])
        ]
    Result err ok ->
      customTypeEncoder
        [ (VariableName (Text.pack "Err"), [err])
        , (VariableName (Text.pack "Ok"), [ok])
        ]
    Tuple2 _a _b -> undefined
    Tuple3 _a _b _c -> undefined
    Record _fields -> undefined
    Lambda _i _o -> undefined
    Defined _name -> undefined

customTypeEncoder :: [(VariableName, [ElmValue])] -> ElmValue
customTypeEncoder ctors =
  mkLambda [Match (VariableName (Text.pack "x")) []] . mkCase (fn "x") $
  uncurry constructorEncoder <$> ctors

rightPizza :: ElmValue -> ElmValue -> ElmValue
rightPizza left right = fn "Basics.|>" left right

leftPizza :: ElmValue -> ElmValue -> ElmValue
leftPizza left right = fn "Basics.<|" left right

constructorEncoder :: VariableName -> [ElmValue] -> (Pattern, ElmValue)
constructorEncoder name paramEncoders =
  let vars =
        take (length paramEncoders) $
        (VariableName . Text.pack . ("x" <>) . show) <$> ([1 ..] :: [Int])
   in ( Match name (flip Match [] <$> vars)
      , recordEncoder
          [ ( Text.pack "ctor"
            , fn "Json.Encode.string" (mkString (unVariableName name)))
          , ( Text.pack "value"
            , mkList $
              zipWith
                (\param encoder -> encoder `leftPizza` mkVar param)
                vars
                paramEncoders)
          ])

recordEncoder :: [(Text, ElmValue)] -> ElmValue
recordEncoder fields =
  fn "Json.Encode.object" (mkList $ uncurry fieldEncoder <$> fields)
  where
    fieldEncoder :: Text -> ElmValue -> ElmValue
    fieldEncoder name = mkTuple2 (mkString name)
