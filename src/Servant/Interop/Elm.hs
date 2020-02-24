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
  ( printModule,
  )
where

import Data.List (intersperse)
import Data.Proxy (Proxy)
import Data.Text (Text)
import qualified Servant.Interop
import qualified Servant.Interop.Elm.Generate as Generate
import Servant.Interop.Elm.Types
import Servant.Interop.Elm.Values (ElmFunction, printFunction)
import qualified Text.PrettyPrint.Leijen.Text as PP
import qualified Wire

printModule :: Servant.Interop.HasWireFormat a => Proxy a -> Text
printModule api =
  printDoc . printModule' . Module $ clients <> helpers
  where
    (userTypes, endpoints) = Servant.Interop.wireFormat api
    clients = FunctionDefinition . Generate.generateClient <$> endpoints
    helpers =
      foldMap (uncurry definitionsForType)
        . sortUserTypes
        . fst
        . fromWireUserTypes
        $ userTypes

newtype Module = Module [Definition]

data Definition
  = TypeDefinition Wire.TypeName ElmTypeDefinition
  | FunctionDefinition ElmFunction

printModule' :: Module -> PP.Doc
printModule' (Module definitions) =
  PP.vcat $ intersperse PP.linebreak $ printDefinition <$> definitions
  where
    printDefinition =
      \case
        TypeDefinition name t ->
          printTypeDefinition name t
        FunctionDefinition f ->
          printFunction f

definitionsForType :: Wire.TypeName -> ElmTypeDefinition -> [Definition]
definitionsForType name definition =
  [ TypeDefinition name definition,
    FunctionDefinition $ Generate.generateEncoder name definition,
    FunctionDefinition $ Generate.generateDecoder name definition
  ]

printDoc :: PP.Doc -> Text
printDoc = PP.displayTStrict . PP.renderPretty 1 80
