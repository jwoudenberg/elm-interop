{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Main
  ( main,
  )
where

import Data.Proxy (Proxy (Proxy))
import qualified Examples.Roundtrip
import qualified Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty
import qualified Test.Tasty.Hedgehog
import qualified Wire
import qualified Wire.Json

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "Wire unit"
    [ repDualityTests,
      jsonDualityTests
    ]

repDualityTests :: TestTree
repDualityTests =
  Test.Tasty.Hedgehog.testProperty "Rep duality" $ Hedgehog.property $ do
    value <- Hedgehog.forAll roundtripValueGenerator
    Hedgehog.tripping value Wire.toWire Wire.fromWire

jsonDualityTests :: TestTree
jsonDualityTests =
  Test.Tasty.Hedgehog.testProperty "Rep duality" $ Hedgehog.property $ do
    let coder = Wire.Json.coderForType (Wire.wireType (Proxy :: Proxy Examples.Roundtrip.Value))
    input <- Hedgehog.forAll (Wire.toWire <$> roundtripValueGenerator)
    encoded <- maybe Hedgehog.failure pure (Wire.Json.encodeJson coder input)
    Hedgehog.annotateShow encoded
    output <- maybe Hedgehog.failure pure (Wire.Json.decodeJson coder encoded)
    (Hedgehog.===) input output

roundtripValueGenerator :: Hedgehog.Gen Examples.Roundtrip.Value
roundtripValueGenerator =
  Examples.Roundtrip.Record
    <$> Gen.int (Range.linear 0 100)
    <*> Gen.text (Range.linear 0 100) Gen.unicode
