module ElmType where

import Data.HashMap.Strict (HashMap)
import Data.Text (Text)

data ElmType
  = ElmInt
  | ElmString
  | ElmList ElmType
  | ElmTuple2 ElmType
              ElmType
  | ElmResult { ok :: ElmType
              , err :: ElmType }
  | ElmRecord (HashMap Text ElmType)
  deriving (Show)
