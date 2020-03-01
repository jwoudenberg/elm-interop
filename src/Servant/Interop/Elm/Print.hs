{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Servant.Interop.Elm.Print where

import Data.Text.Prettyprint.Doc ((<+>))
import qualified Data.Text.Prettyprint.Doc as PP

type Doc = PP.Doc ()

-- |
-- Replacement for the `PP.<$>` operator, which we use for `fmap` instead.
infixr 5 <++>

(<++>) :: Doc -> Doc -> Doc
(<++>) x y = x <> PP.line <> y

elmIndent :: Int
elmIndent = 4

-- |
-- Version of `encloseSep` that puts the closing delimiter on a new line, and
-- adds a space between the separator and the content.
--
-- Used for printing lists and records in a fashion compatible with elm-format.
encloseSep' :: Doc -> Doc -> Doc -> [Doc] -> Doc
encloseSep' left right sp ds =
  case ds of
    [] -> left <> right
    _ -> PP.vcat entries <++> right
      where
        entries = zipWith (\pre x -> pre <+> PP.align x) (left : repeat sp) ds

-- |
-- Switch between hanging formatting or single-line formatting.
--
-- Hanging:
--
--     line1
--        line2
--        line3
--
-- Single-line notation:
--
--     line1 line2 line3
hangCollapse :: Doc -> Doc
hangCollapse = PP.nest elmIndent . PP.group

data TypeAppearance
  = -- | The printed type consists of a single word, like `Int` or `Thing`.
    SingleWord
  | -- | The printed type consists of multiple words, like `List Int`
    MultipleWord
  | -- | The type is a lambda, like `Text -> Int`. Implies `MultipleWord`.
    MultipleWordLambda

parens :: TypeAppearance -> Doc -> Doc
parens a doc =
  case a of
    SingleWord -> doc
    MultipleWord -> parens' doc
    MultipleWordLambda -> parens' doc

parens' :: Doc -> Doc
parens' doc =
  PP.cat
    [ PP.lparen <> doc,
      PP.rparen
    ]
