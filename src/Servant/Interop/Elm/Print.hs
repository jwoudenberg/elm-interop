{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Servant.Interop.Elm.Print where

import Text.PrettyPrint.Leijen.Text ((<+>))

import qualified Text.PrettyPrint.Leijen.Text as PP

-- |
-- Replacement for the `PP.<$>` operator, which we use for `fmap` instead.
infixr 5 <++>

(<++>) :: PP.Doc -> PP.Doc -> PP.Doc
(<++>) = (PP.<$>)

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
    _ -> PP.vcat entries <++> right
      where entries = zipWith (\pre x -> pre <+> PP.align x) (left : repeat sp) ds

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
--
hangCollapse :: PP.Doc -> PP.Doc
hangCollapse = PP.hang elmIndent . PP.group

data TypeAppearance
  = SingleWord
  -- ^ The printed type consists of a single word, like `Int` or `Thing`.
  | MultipleWord
  -- ^ The printed type consists of multiple words, like `List Int`
  | MultipleWordLambda
  -- ^ The type is a lambda, like `Text -> Int`. Implies `MultipleWord`.

parens :: TypeAppearance -> PP.Doc -> PP.Doc
parens a doc =
  case a of
    SingleWord -> doc
    MultipleWord -> PP.parens doc
    MultipleWordLambda -> PP.parens doc
