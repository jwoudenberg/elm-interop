module Data.Sequence.Extra
  ( mapToSeq
  , mapFromFoldable
  , foldableToSeq
  ) where

import Data.Foldable (foldl')
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Sequence (Seq((:<|)))

mapFromFoldable ::
     Ord k
  => Foldable f =>
       f (k, v) -> Map k v
mapFromFoldable = foldl' (\m (k, v) -> Map.insert k v m) mempty

mapToSeq :: Map k v -> Seq (k, v)
mapToSeq = Map.foldlWithKey' (\s k v -> (k, v) :<| s) mempty

foldableToSeq :: Foldable f => f a -> Seq a
foldableToSeq = foldl' (flip (:<|)) mempty
