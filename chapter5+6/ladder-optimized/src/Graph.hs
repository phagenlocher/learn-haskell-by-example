{-# LANGUAGE ScopedTypeVariables #-}

module Graph where

import qualified Data.HashMap.Lazy as M
import Data.Hashable (Hashable)
import qualified Data.List as L
import Data.Maybe (isJust)

type DiGraph a = M.HashMap a [a]

empty :: DiGraph a
empty = M.empty

addNode :: (Hashable a, Eq a) => a -> DiGraph a -> DiGraph a
addNode =
  M.alter
    ( \mNodes ->
        case mNodes of
          Nothing -> Just []
          value -> value
    )

addEdge :: (Hashable a, Eq a) => (a, a) -> DiGraph a -> DiGraph a
addEdge (node, child) = M.alter insertEdge node
  where
    insertEdge Nothing = Just [child]
    insertEdge (Just nodes) =
      Just (L.nub (child : nodes))

addEdges :: (Hashable a, Eq a) => [(a, a)] -> DiGraph a -> DiGraph a
addEdges [] graph = graph
addEdges (edge : edges) graph = addEdge edge (addEdges edges graph)

buildDiGraph :: (Hashable a, Eq a) => [(a, [a])] -> DiGraph a
buildDiGraph nodes = go nodes M.empty
  where
    go [] graph = graph
    go ((key, value) : xs) graph = M.insert key value (go xs graph)

children :: (Hashable a, Eq a) => a -> DiGraph a -> [a]
children = M.findWithDefault []

deleteNode :: (Hashable a, Eq a) => a -> DiGraph a -> DiGraph a
deleteNode = M.delete

deleteNodes :: (Hashable a, Eq a) => [a] -> DiGraph a -> DiGraph a
deleteNodes [] graph = graph
deleteNodes (x : xs) graph = M.delete x (deleteNodes xs graph)

deleteEdge :: (Hashable a, Eq a) => (a, a) -> DiGraph a -> DiGraph a
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

bfsSearch :: forall a. (Hashable a, Eq a) => DiGraph a -> a -> a -> Maybe [a]
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

type BiSearchState a = (SearchState a, SearchState a)

biBfsSearch :: forall a. (Hashable a, Eq a) => DiGraph a -> a -> a -> Maybe [a]
biBfsSearch graph start end
  | start == end = Just [start]
  | otherwise =
    let fState = ([start], graph, empty)
        bState = ([end], graph, empty)
     in biBfsSearch' (fState, bState)
  where
    findSolution :: DiGraph a -> a -> [a]
    findSolution g = go
      where
        go x =
          case children x g of
            [] -> [x]
            (v : _) -> x : go v

    checkOverlap :: BiSearchState a -> Maybe [a]
    checkOverlap ((fFrontier', _, fPreds'), (bFrontier', _, bPreds')) =
      let getSolution x =
            let fPath = findSolution fPreds' x
                bPath = findSolution bPreds' x
             in L.reverse fPath ++ L.tail bPath
          overlaps =
            L.filter (`M.member` bPreds') fFrontier'
          solutions =
            L.sortOn L.length (L.map getSolution overlaps)
       in if L.null solutions
            then Nothing
            else Just $ L.head solutions

    checkSolution :: SearchState a -> a -> Maybe [a]
    checkSolution (frontier, _, preds) node
      | node `L.elem` frontier = Just (findSolution preds node)
      | otherwise = Nothing

    biBfsSearch' :: BiSearchState a -> Maybe [a]
    biBfsSearch' state@(fState@(fFrontier, _, _), bState@(bFrontier, _, _))
      | L.null fFrontier && L.null bFrontier = Nothing
      | isJust fSol = fmap L.reverse fSol
      | isJust bSol = bSol
      | isJust overlapSol = overlapSol
      | otherwise = biBfsSearch' biState'
      where
        fSol = checkSolution fState end
        bSol = checkSolution bState start
        overlapSol = checkOverlap state
        fState' = bfsSearchStep fState
        bState' = bfsSearchStep bState
        biState' = (fState', bState')

    addMultiplePredecessors :: [(a, [a])] -> DiGraph a -> DiGraph a
    addMultiplePredecessors [] g = g
    addMultiplePredecessors ((n, ch) : xs) g =
      addMultiplePredecessors xs (go n ch g)
      where
        go n [] g = g
        go n (x : xs) g = go n xs (addEdge (x, n) g)

    bfsSearchStep :: SearchState a -> SearchState a
    bfsSearchStep (frontier, g, preds) =
      let g' = deleteNodes frontier g
          ch =
            L.map
              (\n -> (n, L.filter (`M.member` g') (children n g)))
              frontier
          frontier' = L.concatMap snd ch
          preds' = addMultiplePredecessors ch preds
       in (frontier', g', preds')
