{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Data.Vinyl.POP
  ( POP
  , rpureConstrained
  , AllFieldsAllConstrained
  ) where

import Data.Proxy (Proxy(Proxy))
import Data.Vinyl (Rec((:&), RNil), RecApplicative, rpure)
import Data.Vinyl.Record
  ( AllFieldsConstrained
  , Field(Field)
  , Record
  , RecordApplicative
  )
import Data.Vinyl.TypeLevel (AllConstrained)
import GHC.Exts (Constraint)

import qualified Data.Vinyl.Record as Record

type POP f = Record (Rec f)

type family AllFieldsAllConstrained c xss :: Constraint where
  AllFieldsAllConstrained c '[] = ()
  AllFieldsAllConstrained c ('( s, xs) ': xss) = ( AllConstrained c xs
                                                 , AllFieldsAllConstrained c xss)

rpureConstrained ::
     forall c f proxy xss.
     ( AllFieldsAllConstrained c xss
     , AllFieldsConstrained RecApplicative xss
     , RecordApplicative xss
     )
  => proxy c
  -> (forall a. c a =>
                  f a)
  -> POP f xss
rpureConstrained _ f = cols proxies
  where
    cols ::
         ( AllFieldsAllConstrained c xss'
         , AllFieldsConstrained RecApplicative xss'
         )
      => POP Proxy xss'
      -> POP f xss'
    cols RNil = RNil
    cols (Field xs :& xss) = Field (rows xs) :& cols xss
    rows :: (AllConstrained c xs') => Rec Proxy xs' -> Rec f xs'
    rows RNil = RNil
    rows (Proxy :& xs) = f :& rows xs

proxies ::
     forall xss.
     (RecordApplicative xss, AllFieldsConstrained RecApplicative xss)
  => POP Proxy xss
proxies = go $ Record.rpure Proxy
  where
    go ::
         (AllFieldsConstrained RecApplicative xss')
      => Record Proxy xss'
      -> POP Proxy xss'
    go RNil = RNil
    go (Field Proxy :& xss) = Field (rpure Proxy) :& go xss
