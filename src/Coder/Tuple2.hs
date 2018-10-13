{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeApplications #-}

module Coder.Tuple2
  ( tuple2
  ) where

import Coder.Internal (Coder)
import Coder.Record (record)
import Data.Functor.Invariant (Invariant(invmap))
import Data.Vinyl (Label(Label), Rec((:&), RNil))
import Data.Vinyl.Functor (Identity(getIdentity))
import Data.Vinyl.Record (Record, (=:), getField)

tuple2 :: Coder a -> Coder b -> Coder (a, b)
tuple2 first second =
  invmap fromRecord toRecord . record $
  (Label @"_1" =: first) :& (Label @"_2" =: second) :& RNil
  where
    toRecord (a, b) = (Label @"_1" =: pure a) :& (Label @"_2" =: pure b) :& RNil
    fromRecord :: Record Identity '[ '( "_1", a), '( "_2", b)] -> (a, b)
    fromRecord (x :& y :& RNil) =
      (getIdentity $ getField x, getIdentity $ getField y)
