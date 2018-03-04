module ElmType.TypeAST where

import Data.Text

data AST
  = Int
  | String
  | Alias Text
          AST
  | Record [(Text, AST)]
  | Union Text
          [(Text, [AST])]
