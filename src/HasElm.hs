{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module HasElm
  ( HasElm(hasElm)
  ) where

import Data.Proxy (Proxy(Proxy))
import Data.Vinyl (Label(Label), RecApplicative)
import Data.Vinyl.CoRec (FoldRec)
import Data.Vinyl.Functor (Identity)
import Data.Vinyl.POP (AllFieldsAllConstrained, POP)
import Data.Vinyl.Record
  ( AllFieldsConstrained
  , Record
  , RecordApplicative
  , rpureConstrained
  )
import Data.Vinyl.SOP (SOP)
import GHC.TypeLits (KnownSymbol)
import ToElm (ToElm)

import qualified Data.Vinyl.POP as POP
import qualified ToElm

class HasElm a where
  hasElm :: ToElm a

instance HasElm Int where
  hasElm = ToElm.int

instance HasElm a => HasElm [a] where
  hasElm = ToElm.list hasElm

instance (HasElm a, HasElm b) => HasElm (a, b) where
  hasElm = ToElm.tuple2 hasElm hasElm

instance (HasElm l, HasElm r) => HasElm (Either l r) where
  hasElm = ToElm.either hasElm hasElm

instance forall xs. (AllFieldsConstrained HasElm xs, RecordApplicative xs) =>
         HasElm (Record Identity xs) where
  hasElm = ToElm.record (recSpec (Proxy @xs))

recSpec ::
     (AllFieldsConstrained HasElm xs, RecordApplicative xs)
  => Proxy xs
  -> Record ToElm xs
recSpec _ = rpureConstrained (Proxy @HasElm) hasElm

instance forall s xs xss. ( KnownSymbol s
                          , AllFieldsAllConstrained HasElm (xs ': xss)
                          , AllFieldsConstrained RecApplicative (xs ': xss)
                          , RecordApplicative (xs ': xss)
                          , FoldRec (xs ': xss) (xs ': xss)
         ) =>
         HasElm (Label s, SOP Identity (xs ': xss)) where
  hasElm = ToElm.custom (Label @s, customSpec (Proxy @(xs ': xss)))

customSpec ::
     ( AllFieldsConstrained RecApplicative xss
     , AllFieldsAllConstrained HasElm xss
     , RecordApplicative xss
     )
  => Proxy xss
  -> POP ToElm xss
customSpec _ = POP.rpureConstrained (Proxy @HasElm) hasElm
