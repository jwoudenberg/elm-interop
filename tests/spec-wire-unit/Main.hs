{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Main
  ( main,
  )
where

import qualified Examples.Roundtrip
import qualified Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty
import qualified Test.Tasty.Hedgehog
import qualified Wire

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "Wire unit"
    [ repDualityTests
    ]

repDualityTests :: TestTree
repDualityTests =
  Test.Tasty.Hedgehog.testProperty "Rep duality" $ Hedgehog.property $ do
    value <- Hedgehog.forAll roundtripValueGenerator
    Hedgehog.tripping value Wire.toWire Wire.fromWire

roundtripValueGenerator :: Hedgehog.Gen Examples.Roundtrip.Value
roundtripValueGenerator =
  Examples.Roundtrip.Record
    <$> Gen.int (Range.linear 0 100)
    <*> Gen.text (Range.linear 0 100) Gen.unicode
