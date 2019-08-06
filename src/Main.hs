{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Main
  ( main
  ) where

import Data.Functor.Foldable (Fix, ana, unfix, zygo)
import Data.HashMap.Strict.InsOrd (InsOrdHashMap)
import Data.Text (Text)
import Dhall.Core (Expr)
import Dhall.TypeCheck (X)
import Text.PrettyPrint.Leijen.Text ((<+>))

import qualified Data.HashMap.Strict.InsOrd as HashMap
import qualified Data.Text as Text
import qualified Dhall
import qualified Dhall.Core
import qualified Text.PrettyPrint.Leijen.Text as PP

-- |
-- Small test of functionality in this library. Will be removed before release.
main :: IO ()
main = do
  dhall <-
    Dhall.inputExpr
      "{ is : List (Optional Integer), d : Double, n : List Natural -> Text -> Bool }"
  let elmType = dhallToElm dhall
  putStrLn . Text.unpack . showDoc . printTypeDefinition $ Alias "Hi" elmType

data ElmTypeF a
  = Unit
  | Bool
  | Int
  | Float
  | String
  | List a
  | Maybe a
  | Record (InsOrdHashMap Text a)
  | Lambda a
           a
  | Defined (ElmTypeDefinitionF a)
  deriving (Functor, Show)

type ElmType = Fix ElmTypeF

data ElmTypeDefinitionF a
  = Custom Text
           (InsOrdHashMap Text [a])
  | Alias Text
          a
  deriving (Functor, Show)

type ElmTypeDefinition = ElmTypeDefinitionF ElmType

showDoc :: PP.Doc -> Text
showDoc = PP.displayTStrict . PP.renderPretty 1 40

printType :: ElmType -> PP.Doc
printType =
  zygo appearance $ \case
    Unit -> "()"
    Bool -> "Bool"
    Int -> "Int"
    Float -> "Float"
    String -> "String"
    List (a, i) -> "List" <+> parens a i
    Maybe (a, i) -> "Maybe" <+> parens a i
    Record xs -> encloseSep' PP.lbrace PP.rbrace PP.comma fields
      where fields = printRecordField <$> HashMap.toList xs
    Defined (Custom n _) -> PP.textStrict n
    Defined (Alias n _) -> PP.textStrict n
    Lambda (ai, i) (_, o) -> idoc <+> "->" <+> o
      -- |
      -- We only need to parenthesize the input argument if it is a
      -- lambda function itself.
      where idoc =
              case ai of
                SingleWord -> i
                MultipleWord -> i
                MultipleWordLambda -> PP.parens i

printTypeDefinition :: ElmTypeDefinition -> PP.Doc
printTypeDefinition =
  \case
    Custom name constructors ->
      "type" <+> PP.textStrict name <> PP.linebreak <> printedConstructors
      where printedConstructors =
              PP.indent elmIndent . PP.vcat . zipWith (<+>) ("=" : repeat "|") $
              printConstructor <$> HashMap.toList constructors
    Alias name base ->
      "type alias" <+>
      PP.textStrict name <+>
      "=" <> PP.line <> PP.indent elmIndent (printType base)

printConstructor :: (Text, [ElmType]) -> PP.Doc
printConstructor (name, params) =
  PP.nest elmIndent (PP.sep (PP.textStrict name : (printParam <$> params)))
  where
    printParam :: ElmType -> PP.Doc
    printParam t =
      case appearance (unfix t) of
        SingleWord -> printType t
        MultipleWord -> PP.parens $ printType t
        MultipleWordLambda -> PP.parens $ printType t

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
    [d] -> left <+> d <+> right
    _ -> PP.group $ PP.vcat entries <> PP.line <> right
      where entries = zipWith (<+>) (left : repeat sp) ds

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
    Bool -> SingleWord
    Int -> SingleWord
    Float -> SingleWord
    String -> SingleWord
    List _ -> MultipleWord
    Maybe _ -> MultipleWord
    Record _ -> SingleWord
    Defined _ -> SingleWord
    Lambda _ _ -> MultipleWordLambda

parens :: TypeAppearance -> PP.Doc -> PP.Doc
parens a doc =
  case a of
    SingleWord -> doc
    MultipleWord -> PP.parens doc
    MultipleWordLambda -> PP.parens doc

printRecordField :: (Text, (a, PP.Doc)) -> PP.Doc
printRecordField (k, (_, v)) = PP.textStrict k <+> ":" <+> v

dhallToElm :: Expr s X -> ElmType
dhallToElm =
  ana $ \case
    Dhall.Core.Pi _var x y -> Lambda x y
    Dhall.Core.Bool -> Bool
    Dhall.Core.Natural -> Int
    Dhall.Core.Integer -> Int
    Dhall.Core.Double -> Float
    Dhall.Core.Text -> String
    Dhall.Core.App Dhall.Core.List x -> List x
    Dhall.Core.App Dhall.Core.Optional x -> Maybe x
    Dhall.Core.Record xs -> Record xs
    Dhall.Core.Union xs -> Defined (Custom name (fmap pure xs))
      where name = Text.intercalate "Or" $ HashMap.keys xs
    Dhall.Core.CombineTypes (Dhall.Core.Record xs) (Dhall.Core.Record ys) ->
      Record (HashMap.unionWith Dhall.Core.CombineTypes xs ys)
    _ -> Unit
