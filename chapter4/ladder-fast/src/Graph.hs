{-# LANGUAGE ScopedTypeVariables #-}

module Graph where

import qualified Data.HashMap.Lazy as M
import Data.Hashable (Hashable)
import qualified Data.List as L

type DiGraph a = M.HashMap a [a]

empty :: DiGraph a
empty = M.empty

addNode :: (Eq a, Hashable a) => a -> DiGraph a -> DiGraph a
addNode =
  M.alter
    ( \mNodes ->
        case mNodes of
          Nothing -> Just []
          value -> value
    )

addEdge :: (Eq a, Hashable a) => (a, a) -> DiGraph a -> DiGraph a
addEdge (node, child) = M.alter insertEdge node
  where
    insertEdge Nothing = Just [child]
    insertEdge (Just nodes) =
      Just (L.nub (child : nodes))

addEdges :: (Eq a, Hashable a) => [(a, a)] -> DiGraph a -> DiGraph a
addEdges [] graph = graph
addEdges (edge : edges) graph = addEdge edge (addEdges edges graph)

buildDiGraph :: (Eq a, Hashable a) => [(a, [a])] -> DiGraph a
buildDiGraph nodes = go nodes M.empty
  where
    go [] graph = graph
    go ((key, value) : xs) graph = M.insert key value (go xs graph)

children :: (Eq a, Hashable a) => a -> DiGraph a -> [a]
children = M.findWithDefault []

deleteNode :: (Eq a, Hashable a) => a -> DiGraph a -> DiGraph a
deleteNode = M.delete

deleteNodes :: (Eq a, Hashable a) => [a] -> DiGraph a -> DiGraph a
deleteNodes [] graph = graph
deleteNodes (x : xs) graph = M.delete x (deleteNodes xs graph)

deleteEdge :: (Eq a, Hashable a) => (a, a) -> DiGraph a -> DiGraph a
deleteEdge (node, child) =
  M.alter
    ( \mNodes ->
        case mNodes of
          Nothing -> Just []
          Just nodes ->
            Just (L.delete child nodes)
    )
    node

type SearchState a = ([a], DiGraph a, DiGraph a)

data SearchResult a = Unsuccessful | Successful (DiGraph a)

bfsSearch :: forall a. (Eq a, Hashable a) => DiGraph a -> a -> a -> Maybe [a]
bfsSearch graph start end
  | start == end = Just [start]
  | otherwise =
    case bfsSearch' ([start], graph, empty) of
      Successful preds -> Just (findSolution preds)
      Unsuccessful -> Nothing
  where
    findSolution :: DiGraph a -> [a]
    findSolution g = L.reverse (go end)
      where
        go x =
          case children x g of
            [] -> [x]
            (v : _) -> x : go v

    addMultiplePredecessors :: [(a, [a])] -> DiGraph a -> DiGraph a
    addMultiplePredecessors [] g = g
    addMultiplePredecessors ((n, ch) : xs) g =
      addMultiplePredecessors xs (go n ch g)
      where
        go n [] g = g
        go n (x : xs) g = go n xs (addEdge (x, n) g)

    bfsSearch' :: SearchState a -> SearchResult a
    bfsSearch' ([], _, preds) = Unsuccessful
    bfsSearch' (frontier, g, preds) =
      let g' = deleteNodes frontier g
          ch =
            L.map
              (\n -> (n, L.filter (`M.member` g') (children n g)))
              frontier
          frontier' = L.concatMap snd ch
          preds' = addMultiplePredecessors ch preds
       in if end `L.elem` frontier'
            then Successful preds'
            else bfsSearch' (frontier', g', preds')
