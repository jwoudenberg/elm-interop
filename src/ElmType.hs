module ElmType where

import Data.Functor.Foldable (Fix)
import Data.HashMap.Strict (HashMap)
import Data.Text (Text)

type ElmType = Fix ElmTypeF

data ElmTypeF a
  = ElmInt
  | ElmString
  | ElmList a
  | ElmTuple2 a
              a
  | ElmResult { ok :: a
              , err :: a }
  | ElmRecord (HashMap Text a)
  | ElmCustomType { name :: Text
                  , constructors :: HashMap Text [a] }
  deriving (Show)
