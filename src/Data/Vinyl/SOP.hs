module Data.Vinyl.SOP
  ( SOP
  ) where

import Data.Vinyl (Rec)
import Data.Vinyl.Sum (Sum)

type SOP f = Sum (Rec f)
