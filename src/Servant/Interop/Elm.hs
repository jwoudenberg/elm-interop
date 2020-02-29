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

import Data.List (intersperse)
import Data.Proxy (Proxy)
import qualified Data.Text as T
import qualified Servant.Interop
import qualified Servant.Interop.Elm.Generate as Generate
import Servant.Interop.Elm.Types
import Servant.Interop.Elm.Values (ElmFunction, printFunction)
import qualified Text.PrettyPrint.Leijen.Text as PP
import qualified Wire

data Options
  = Options
      { domain :: T.Text,
        moduleName :: T.Text
      }

printModule :: Servant.Interop.HasWireFormat a => Options -> Proxy a -> T.Text
printModule options api =
  printDoc . printModule' . Module (moduleName options) $ clients <> helpers
  where
    (userTypes, endpoints) = Servant.Interop.wireFormat api
    clients = FunctionDefinition . Generate.generateClient (domain options) <$> endpoints
    helpers =
      foldMap (uncurry definitionsForType)
        . sortUserTypes
        . fst
        . fromWireUserTypes
        $ userTypes

data Module
  = Module
      { name :: T.Text,
        definitions :: [Definition]
      }

data Definition
  = TypeDefinition Wire.TypeName ElmTypeDefinition
  | FunctionDefinition ElmFunction

printModule' :: Module -> PP.Doc
printModule' module_ =
  printedHeader
    <> PP.linebreak
    <> PP.linebreak
    <> printedImports
    <> PP.linebreak
    <> PP.linebreak
    <> PP.linebreak
    <> printedDefinitions
  where
    printedHeader :: PP.Doc
    printedHeader = PP.sep [PP.textStrict "module", PP.textStrict (name module_), PP.textStrict "exposing (..)"]
    printedImports :: PP.Doc
    printedImports =
      PP.vcat $
        PP.textStrict
          <$> [ "import Http",
                "import Json.Decode",
                "import Json.Encode"
              ]
    printedDefinitions :: PP.Doc
    printedDefinitions = PP.vcat $ intersperse PP.linebreak $ printDefinition <$> definitions module_
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

printDoc :: PP.Doc -> T.Text
printDoc = PP.displayTStrict . PP.renderPretty 1 80
