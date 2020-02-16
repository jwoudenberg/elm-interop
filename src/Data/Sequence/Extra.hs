module Data.Sequence.Extra
  ( mapToSeq,
    hashMapToSeq,
    mapFromFoldable,
    hashMapFromFoldable,
    foldableToSeq,
  )
where

import Data.Foldable (foldl')
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Hashable (Hashable)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Sequence (Seq ((:<|)))

mapFromFoldable ::
  Ord k =>
  Foldable f =>
  f (k, v) ->
  Map k v
mapFromFoldable = foldl' (\m (k, v) -> Map.insert k v m) mempty

mapToSeq :: Map k v -> Seq (k, v)
mapToSeq = Map.foldlWithKey' (\s k v -> (k, v) :<| s) mempty

hashMapFromFoldable ::
  (Eq k, Hashable k) =>
  Foldable f =>
  f (k, v) ->
  HashMap k v
hashMapFromFoldable = foldl' (\m (k, v) -> HashMap.insert k v m) mempty

hashMapToSeq :: HashMap k v -> Seq (k, v)
hashMapToSeq = HashMap.foldlWithKey' (\s k v -> (k, v) :<| s) mempty

foldableToSeq :: Foldable f => f a -> Seq a
foldableToSeq = foldl' (flip (:<|)) mempty
