module PermutationMap where

import qualified Data.ByteString as BS
import qualified Data.HashMap.Lazy as M
import Data.Maybe (fromMaybe)

type PermutationMap = M.HashMap BS.ByteString [BS.ByteString]

empty :: PermutationMap
empty = M.empty

member :: BS.ByteString -> PermutationMap -> Bool
member key = M.member (BS.sort key)

alter ::
  ( Maybe [BS.ByteString] ->
    Maybe [BS.ByteString]
  ) ->
  BS.ByteString ->
  PermutationMap ->
  PermutationMap
alter f key = M.alter f (BS.sort key)

delete :: BS.ByteString -> PermutationMap -> PermutationMap
delete key = M.delete (BS.sort key)

insert :: BS.ByteString -> [BS.ByteString] -> PermutationMap -> PermutationMap
insert key = M.insert (BS.sort key)

lookup :: BS.ByteString -> PermutationMap -> Maybe [BS.ByteString]
lookup key = M.lookup (BS.sort key)

findWithDefault ::
  [BS.ByteString] ->
  BS.ByteString ->
  PermutationMap ->
  [BS.ByteString]
findWithDefault defaultValue key map =
  fromMaybe [] (PermutationMap.lookup key map)

createPermutationMap :: [BS.ByteString] -> PermutationMap
createPermutationMap = go empty
  where
    go permMap [] = permMap
    go permMap (x : xs) = go (insertPermutation x permMap) xs

    insertPermutation word = alter (insertList word) word

    insertList word Nothing = Just [word]
    insertList word (Just words) = Just $ word : words
