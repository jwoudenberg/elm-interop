{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Named where

import Data.Proxy
import Data.Text
import GHC.TypeLits

newtype Named s t = Named
  { unNamed :: t
  }

name ::
     forall s t. KnownSymbol s
  => Proxy (Named s t)
  -> Text
name _ = pack . symbolVal $ Proxy @s
