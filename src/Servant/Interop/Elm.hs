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
--
module Servant.Interop.Elm
  ( printModule
  ) where

import qualified Servant.Interop.Elm.Generate as Generate
import qualified Wire
import Data.List (intersperse)
import Data.Proxy (Proxy)
import Data.Text (Text)
import Servant.Interop.Elm.Types

import qualified Servant.Interop
import qualified Text.PrettyPrint.Leijen.Text as PP

printModule :: Servant.Interop.HasWireFormat a => Proxy a -> Text
printModule =
  printDoc .
  PP.vcat .
  intersperse PP.linebreak .
  fmap (uncurry printForType) .
  sortUserTypes . fst . fromWireUserTypes . fst . Servant.Interop.wireFormat

printForType :: Wire.TypeName -> ElmTypeDefinition -> PP.Doc
printForType name definition =
  PP.vcat $ intersperse PP.linebreak $ 
    [ printTypeDefinition name definition
    , Generate.printEncoder name definition
    , Generate.printDecoder name definition
    ]

printDoc :: PP.Doc -> Text
printDoc = PP.displayTStrict . PP.renderPretty 1 80
