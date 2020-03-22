{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Main
  ( main,
  )
where

import Data.Proxy (Proxy (Proxy))
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import qualified Examples.Roundtrip
import qualified Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Main
import qualified Hedgehog.Range as Range
import qualified Wire
import qualified Wire.Json

main :: IO ()
main = Hedgehog.Main.defaultMain [tests]

tests :: IO Bool
tests =
  Hedgehog.checkParallel $
    Hedgehog.Group
      "Wire unit"
      [ ("Wire.toWire / Wire.fromWire duality", repDuality),
        ("Wire.Json.encodeJson / Wire.Json.decodeJson duality", jsonDuality)
      ]

repDuality :: Hedgehog.Property
repDuality =
  Hedgehog.property $ do
    value <- Hedgehog.forAll roundtripValueGenerator
    Hedgehog.tripping value Wire.toWire Wire.fromWire

jsonDuality :: Hedgehog.Property
jsonDuality =
  Hedgehog.property $ do
    let coder = Wire.Json.coderForType (Wire.wireType (Proxy :: Proxy Examples.Roundtrip.Value))
    input <- Hedgehog.forAll (Wire.toWire <$> roundtripValueGenerator)
    encoded <- maybe Hedgehog.failure pure (Wire.Json.encodeJson coder input)
    Hedgehog.annotate (TL.unpack (TLE.decodeUtf8 encoded))
    output <- maybe Hedgehog.failure pure (Wire.Json.decodeJson coder encoded)
    (Hedgehog.===) input output

roundtripValueGenerator :: Hedgehog.Gen Examples.Roundtrip.Value
roundtripValueGenerator =
  Examples.Roundtrip.Record
    <$> Gen.int (Range.linearFrom 0 (-100) 100)
    <*> Gen.text (Range.linear 0 100) Gen.unicode
    <*> Gen.list (Range.linear 0 5) Gen.bool
