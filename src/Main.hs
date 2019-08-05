{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveFunctor #-}

module Main where

import Data.Functor.Const (Const(Const))
import Data.Functor.Foldable (Fix(Fix), ana)
import Data.HashMap.Strict.InsOrd (InsOrdHashMap)
import Data.Text (Text)
import Dhall.Core (Expr)
import Dhall.TypeCheck (X)

import qualified Data.HashMap.Strict.InsOrd as HashMap
import qualified Data.Text as Text
import qualified Dhall.Core

main :: IO ()
main = putStrLn "hello world"

data CompileError =
  UnsupportedFeature

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
  deriving (Functor)

type ElmType = Fix ElmTypeF

dhallToElm :: Expr s X -> ElmType
dhallToElm e = ana go (Dhall.Core.normalize e)
  where
    go :: Expr t X -> ElmTypeF (Expr t X)
    go e =
      case e of
        Dhall.Core.Pi var x y -> Lambda x y
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
