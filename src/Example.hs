{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module Example where

import Data.Aeson (toJSON)
import Data.Vinyl (Label(Label), Rec((:&), RNil))
import Data.Vinyl.Functor (Identity)
import Data.Vinyl.Record (Field, (=:))
import Serialized (Serialized(Serialized))

type SomeRec = Rec (Field Identity) '[ '( "Foo", Int), '( "Bar", Int)]

x :: SomeRec
x = Label @"Foo" =: pure 42 :& Label @"Bar" =: pure 12 :& RNil

main :: IO ()
main = print (toJSON (Serialized x))
