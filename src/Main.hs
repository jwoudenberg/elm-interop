{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Main
  ( main
  ) where

import Data.Functor.Foldable (Fix, ana, cata)
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
    Dhall.inputExpr "{ is : List (Optional Integer), d : Double, n : Natural }"
  let elmType = dhallToElm dhall
  putStrLn . Text.unpack . showDoc . printType $ elmType

data ElmTypeF a
  = Unit
  | Bool
  | Int
  | Float
  | String
  | List a
  | Maybe a
  | Record (InsOrdHashMap Text a)
  | Union Text
          (InsOrdHashMap Text [a])
  | Lambda a
           a
  deriving (Functor, Show)

type ElmType = Fix ElmTypeF

showDoc :: PP.Doc -> Text
showDoc = PP.displayTStrict . PP.renderPretty 1 120

printType :: ElmType -> PP.Doc
printType =
  cata $ \case
    Unit -> "()"
    Bool -> "Bool"
    Int -> "Int"
    Float -> "Float"
    String -> "String"
    List i -> "List" <+> PP.parens i
    Maybe i -> "Maybe" <+> PP.parens i
    Record xs ->
      PP.encloseSep PP.lbrace PP.rbrace PP.comma . fmap printRecordField $
      HashMap.toList xs
    Union n _ -> PP.textStrict n
    Lambda i o -> PP.parens i <+> "->" <+> PP.parens o

printRecordField :: (Text, PP.Doc) -> PP.Doc
printRecordField (k, v) = PP.textStrict k <+> ":" <+> v

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
    Dhall.Core.Union xs -> Union name (fmap pure xs)
      where name = Text.intercalate "Or" $ HashMap.keys xs
    Dhall.Core.CombineTypes (Dhall.Core.Record xs) (Dhall.Core.Record ys) ->
      Record (HashMap.unionWith Dhall.Core.CombineTypes xs ys)
    _ -> Unit
