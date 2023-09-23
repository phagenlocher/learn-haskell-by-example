module Exercises where

import qualified Data.AssocMap as M
import qualified Data.List as L
import Data.Maybe (catMaybes, isJust)
import Graph (DiGraph (..), addEdge, children)

{-
The `Data.List` module (as well as the always imported `Prelude`) already
provides a function for look up of keys in an associative list called
`lookup`! Rewrite the `member` function by using it!
-}

{-
lookup :: Eq a => a -> [(a, b)] -> Maybe b
This function returns a `Just` of the found value associated with the searched
key. Thus, if we have to check if this result has this constructor which
can be achieved with the `isJust` function.
-}
member :: Eq k => k -> [(k, v)] -> Bool
member key xs = isJust (lookup key xs)

{-
In section 3.3.2 we have learned about the `maybe` function, to work
with `Maybe` values. However, in our implementation of `alter` we have
resorted to manual pattern matching. Rewrite the function to use
`maybe` instead!
-}

{-
A pattern match on Nothing and Just can easily be replaced with `maybe``
by providing the expression in the `Nothing` case as the first argument
and the expression of the `Just` case as a function from the value
to that expression as the second argument.
-}
alter :: Eq k => (Maybe v -> Maybe v) -> k -> [(k, v)] -> [(k, v)]
alter f key [] = maybe [] (\value -> [(key, value)]) (f Nothing)
alter f key ((key', value') : xs)
  | key == key' = maybe xs (\value -> (key, value) : xs) (f (Just value'))
  | otherwise =
    (key', value') : alter f key xs

{-
We have constructed the `member` and `alter` function for the new type by
creating a wrapper for our functions. However, we could have gone through
the work of adding the constructor in the places where we've matched and
constructed lists. As an exercise to get familiar with how to use these
constructors do that instead of using the wrapper!
-}

{-
We recreate the AssocMap type here since it is not exported from
`Data.AssocMap`. We have to unwrap and wrap every operation on the
associative list with the `AssocMap` constructor. In the last case
we use a pattern match in `let` to get to the list in the constructor.
We can do this because there is only a single constructor and we can
be sure that it will match. If we had another constructor there would
be a possibility of crashing!
-}
newtype AssocMap k v = AssocMap [(k, v)]
  deriving (Show)

member' :: Eq k => k -> AssocMap k v -> Bool
member' _ (AssocMap []) = False
member' x (AssocMap ((x', _) : xs))
  | x' == x = True
  | otherwise = member' x (AssocMap xs)

alter' :: Eq k => (Maybe v -> Maybe v) -> k -> AssocMap k v -> AssocMap k v
alter' f key (AssocMap []) =
  case f Nothing of
    Nothing -> AssocMap []
    Just value -> AssocMap [(key, value)]
alter' f key (AssocMap ((key', value') : xs))
  | key == key' =
    case f (Just value') of
      Nothing -> AssocMap xs
      Just value -> AssocMap ((key, value) : xs)
  | otherwise =
    let AssocMap xs' = alter' f key (AssocMap xs)
     in AssocMap ((key', value') : xs')

{-
When taking a good look at the graph functions we can see that the
implementations of `addEdges` and `buildDiGraph` are eerily similar.
Try to generalize these two functions, providing a higher order function
that works with lists in a similar structure and apply this function to
the two definitions. Additionally, we have not provided functions for
deleting nodes and edges from a graph. Implement these functions yourself!
-}

{-
To generalize the functionalities of `addEdges` and `buildDiGraph` we
use a higher-order function that "iterates" over a list and keeps the
result of the compuation in an accumulator called `acc`. The first
argument is a function that computes a new result from a list element
and the accumulator for each "iteration step". Thus we accumulate
some change over the iteration of the list.
-}
accumChange :: (a -> b -> b) -> [a] -> b -> b
accumChange f [] acc = acc
accumChange f (x : xs) acc = accumChange f xs (f x acc)

{-
Using eta-reduction we can very quickly write functions that lift
a function working on a single element with a data structure (e.g.
a single edge and a graph) to a function that works on a list of these
elements and the data structure. We can see this with `addEdges`,
`deleteNodes` and `deleteEdges`.

With `buildDiGraph` we can see that we can perform more actions (like
deconstructing a tuple) in the function given to `accumChange`.
-}
addEdges :: Eq a => [(a, a)] -> DiGraph a -> DiGraph a
addEdges = accumChange addEdge

buildDiGraph :: Eq a => [(a, [a])] -> DiGraph a
buildDiGraph nodes = accumChange (\(k, v) acc -> M.insert k v acc) nodes M.empty

deleteNode :: Eq a => a -> DiGraph a -> DiGraph a
deleteNode = M.delete

deleteNodes :: Eq a => [a] -> DiGraph a -> DiGraph a
deleteNodes = accumChange deleteNode

deleteEdge :: Eq a => (a, a) -> DiGraph a -> DiGraph a
deleteEdge (node, child) =
  M.alter
    ( \mNodes ->
        case mNodes of
          Nothing -> Just []
          Just nodes ->
            Just (L.delete child nodes)
    )
    node

deleteEdges :: Eq a => [(a, a)] -> DiGraph a -> DiGraph a
deleteEdges = accumChange deleteEdge

{-
Have a look at the type expression for the `sort` function. Why can we use
this function on `String` values? Why is it possible for us to use our
polymorphic functions from `AssocMap` in our new module? Try to figure out,
why the types are compatible by inspecting the types using GHCi.
-}

{-
Using GHCi we can get some information on `sort`:

ghci> :t sort
sort :: Ord a => [a] -> [a]
ghci> :i Ord
...
instance Ord a => Ord [a]
...
instance Ord Char
...
ghci> :i String
...
type String = [Char]
...

The `sort` function requires the list to be sorted to contain elements
that have an instance of the `Ord` typeclass. In our case, this type is
`String` which is an alias for `[Char]`. Since `Char` has an instance of
`Ord` and `instance Ord a => Ord [a]` holds, we can infer that an instance
for `[Char]` and thus `String` also exists!

Our `AssocMap` requires us to have an instance of `Eq` for the key type in
order for us to use the functions. Using the same reasoning as before we
find out that `String` also has an instance of `Eq`!

ghci> :i Eq
...
instance Eq a => Eq [a]
...
instance Eq Char
...
-}

{-
For searching the shortest path in a graph we have used _breadth-first search_.
However, other algorithms for searching exist. If we just want to find _any_
path and are not interested in its length the _depth-first search_ might be
suitable. Also, a performance improvement to the ordinary breadth-first search
is the bidirectional breadth-first search, which performs two searches from
both sides at the same time, one searching from start to end and the other
searching from end to start. Once the two searches meet, a solution has been
found. Implement both of these search algorithms!
-}

{-
The depth-first search is much easier to implement than breadth-first search
and can be implemented as a simple recursive function. If the start and end
nodes are the same we have found a simple path. Otherwise, we treat the start
node as the "current" node. We delete it from the graph so that we do not need
to worry about loops. We then use a list comprehension to construct a bunch of
recursive searches for each child of the current node. Remember, that Haskell
is lazy, so these searches are not performed immediately but only if they are
needed. By using `catMaybes` we get rid of all searches that did not return
a path. If none of the searches were successful we return `Nothing`. If there
is atleast one successful search, we simply add our current node to that path.

The bidirectional breadth-first search implementation can be found in the
`ladder-optimized` project, where it is used to search the ladder graph!
-}
dfsSearch :: Eq a => DiGraph a -> a -> a -> Maybe [a]
dfsSearch graph start end
  | start == end = Just [start]
  | otherwise =
    let graph' = deleteNode start graph
        searches = [dfsSearch graph' c end | c <- children start graph]
     in case catMaybes searches of
          [] -> Nothing
          (xs : _) -> Just (start : xs)
