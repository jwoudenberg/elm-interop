{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Generate an Elm module for talking with a Servant API. This will generate
-- code only for endpoints in the API which support the `WIRE` format, see the
-- `Servant.Interop` module for more details.
--
-- Given an API you can generate Elm code for talking with it:
--
--     writeFile
--         "src/Generated.elm"
--         (printModule (Proxy :: Proxy api))
module Servant.Interop.Elm
  ( Options (..),
    printModule,
  )
where

import Data.Proxy (Proxy)
import qualified Data.Text as T
import qualified Data.Text.Prettyprint.Doc as PP
import qualified Data.Text.Prettyprint.Doc.Render.Text as PP.Render
import qualified Servant.Interop
import qualified Servant.Interop.Elm.Generate as Generate
import Servant.Interop.Elm.Print
import Servant.Interop.Elm.Types
import Servant.Interop.Elm.Values (ElmFunction, printFunction)
import qualified Wire

data Options
  = Options
      { domain :: T.Text,
        moduleName :: T.Text
      }

printModule :: Servant.Interop.HasWireFormat a => Options -> Proxy a -> T.Text
printModule options api =
  removeTrailingWhitespace . printDoc . printModule' . Module (moduleName options) $ clients <> helpers
  where
    (userTypes, endpoints) = Servant.Interop.wireFormat api
    clients = FunctionDefinition . Generate.generateClient (domain options) <$> endpoints
    helpers =
      foldMap (uncurry definitionsForType)
        . sortUserTypes
        . fst
        . fromWireUserTypes
        $ userTypes

removeTrailingWhitespace :: T.Text -> T.Text
removeTrailingWhitespace = T.unlines . fmap T.stripEnd . T.lines

data Module
  = Module
      { name :: T.Text,
        definitions :: [Definition]
      }

data Definition
  = TypeDefinition Wire.TypeName ElmTypeDefinition
  | FunctionDefinition ElmFunction

printModule' :: Module -> Doc
printModule' module_ =
  printedHeader
    <> PP.hardline
    <> PP.hardline
    <> printedImports
    <> PP.hardline
    <> PP.hardline
    <> PP.hardline
    <> printedDefinitions
  where
    printedHeader :: Doc
    printedHeader = PP.sep ["module", PP.pretty (name module_), "exposing (..)"]
    printedImports :: Doc
    printedImports =
      PP.vcat $
        [ "import Http",
          "import Json.Decode",
          "import Json.Encode"
        ]
    printedDefinitions :: Doc
    printedDefinitions = mconcat $ PP.punctuate (PP.hardline <> PP.hardline <> PP.hardline) $ printDefinition <$> definitions module_
    printDefinition =
      \case
        TypeDefinition name' t ->
          printTypeDefinition name' t
        FunctionDefinition f ->
          printFunction f

definitionsForType :: Wire.TypeName -> ElmTypeDefinition -> [Definition]
definitionsForType name' definition =
  [ TypeDefinition name' definition,
    FunctionDefinition $ Generate.generateEncoder name' definition,
    FunctionDefinition $ Generate.generateDecoder name' definition
  ]

printDoc :: Doc -> T.Text
printDoc = PP.Render.renderStrict . PP.layoutPretty PP.defaultLayoutOptions
