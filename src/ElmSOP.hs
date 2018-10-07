{-# LANGUAGE GADTs #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module ElmSOP where

import Coder (WrappedField(WrappedField))
import Data.Proxy (Proxy(Proxy))
import Data.Vinyl (AllFields, RecApplicative)

import qualified Data.Vinyl as Vinyl

type ElmSOP f = Vinyl.Rec (WrappedField (Vinyl.Rec f))

rpure ::
     (RecApplicative xs, AllFields xs)
  => (forall a. () =>
                  f a)
  -> ElmSOP f xs
rpure f = Vinyl.rpuref (WrappedField $ Vinyl.rpureConstrained Proxy f)
