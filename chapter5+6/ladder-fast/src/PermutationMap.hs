module PermutationMap where

import qualified Data.HashMap.Lazy as M
import qualified Data.List as L
import Data.Maybe (fromMaybe)

type PermutationMap = M.HashMap String [String]

empty :: PermutationMap
empty = M.empty

member :: String -> PermutationMap -> Bool
member key = M.member (L.sort key)

alter ::
  ( Maybe [String] ->
    Maybe [String]
  ) ->
  String ->
  PermutationMap ->
  PermutationMap
alter f key = M.alter f (L.sort key)

delete :: String -> PermutationMap -> PermutationMap
delete key = M.delete (L.sort key)

insert :: String -> [String] -> PermutationMap -> PermutationMap
insert key = M.insert (L.sort key)

lookup :: String -> PermutationMap -> Maybe [String]
lookup key = M.lookup (L.sort key)

findWithDefault :: [String] -> String -> PermutationMap -> [String]
findWithDefault defaultValue key map =
  fromMaybe [] (PermutationMap.lookup key map)

createPermutationMap :: [String] -> PermutationMap
createPermutationMap = go empty
  where
    go permMap [] = permMap
    go permMap (x : xs) = go (insertPermutation x permMap) xs

    insertPermutation word = alter (insertList word) word

    insertList word Nothing = Just [word]
    insertList word (Just words) = Just $ word : words
