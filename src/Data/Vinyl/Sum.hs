module Data.Vinyl.Sum where

import Data.Vinyl.CoRec (CoRec(CoRec))
import Data.Vinyl.Record (Field(Field))

-- |
-- The counterpart of Vinyls `FieldRec`, but for `CoRec`s. Contains the same
-- custom field type by our custom `Record` type with the same trade-offs.
type Sum f = CoRec (Field f)
