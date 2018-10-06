{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module Example where

import Coder (WrappedField, (=:))
import Data.Aeson (toJSON)
import Data.Vinyl (ElField, Label(Label), Rec((:&), RNil))
import Serialized (Serialized(Serialized))

type SomeRec = Rec ElField '[ '( "Foo", Int), '( "Bar", Int)]

x :: SomeRec
x = Label @"Foo" =: 42 :& Label @"Bar" =: 12 :& RNil

main :: IO ()
main = print (toJSON (Serialized x))
