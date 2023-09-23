module Ladder where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C
import qualified Data.List as L
import qualified Graph as G
import qualified PermutationMap as PM

type Dictionary = [BS.ByteString]

readDictionary :: FilePath -> IO Dictionary
readDictionary filepath = do
  dictionaryContent <- C.readFile filepath
  let lines = C.lines dictionaryContent
      words = L.map (C.filter (`L.elem` ['a' .. 'z'])) lines
  return words

delete :: Char -> BS.ByteString -> BS.ByteString
delete ch string = case C.uncons string of
  Just (x, xs) -> if ch == x then xs else C.cons x (delete ch xs)
  Nothing -> C.empty

computeCandidates :: PM.PermutationMap -> BS.ByteString -> [BS.ByteString]
computeCandidates map word =
  let candidates = modified ++ removed ++ added ++ [word]
      perms =
        L.concatMap
          (\x -> PM.findWithDefault [] x map)
          candidates
   in L.filter (/= word) (L.nub perms)
  where
    added = [C.cons x word | x <- ['a' .. 'z']]
    removed = [delete x word | x <- C.unpack word]
    modified =
      [C.cons x (delete y word) | x <- ['a' .. 'z'], y <- C.unpack word, x /= y]

mkLadderGraph :: Dictionary -> G.DiGraph BS.ByteString
mkLadderGraph dict = G.buildDiGraph nodes
  where
    map = PM.createPermutationMap dict
    nodes =
      L.map (\w -> (w, computeCandidates map w)) dict

ladderSolve :: Dictionary -> String -> String -> Maybe [BS.ByteString]
ladderSolve dict start end =
  let g = mkLadderGraph dict
   in G.biBfsSearch g (C.pack start) (C.pack end)
