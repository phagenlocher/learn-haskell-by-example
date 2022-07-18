module Exercises where

{-
 == NOTE ==
  The functions in this module are all suffixed with ' for the simple reason
  we do not want their names to clash with already defined functions in the
  Prelude!
-}

{-
The functions `lines`, `unlines`, `words` and `unwords` are very helpful
utilities when working with strings. Implement these functions yourself
using recursive definitions! Remember that `Strings` are just lists of chars,
so you can pattern match them! Use this with guards in order to construct
these functions! Remember that you can create definitions with more parameters
inside your functions with a let binding!
-}

{-
Implementing lines and words presents us with a tricky problem. We have to
keep track of words that are interrupted by a certain character. However,
pattern matching only allows to look at a single element as the head. So,
whenever we encounter the character we search for, we have already descended
to deep into the recursion and forgot the actual string, we wanted to isolate.

We can solve this by using an additional argument in a locally defined function
that keeps track of that string (called 'line' and 'word' respectively). We add
characters to them until we found the correct character to break up the
strings recursively!

Creating the quasi-inverse functions is a simple recursive algorithm for both
cases.
-}
lines' :: String -> [String]
lines' "" = []
lines' xs =
  let go "" "" = []
      go line "" = [line]
      go line (x:xs)
        | x == '\n' = line : go "" xs
        | otherwise = go (line ++ [x]) xs
   in go "" xs

words' :: String -> [String]
words' xs =
  let go "" "" = []
      go word "" = [word]
      go word (x:xs)
        | x == ' ' = if word /= "" then word : go "" xs else go "" xs
        | otherwise = go (word ++ [x]) xs
   in go "" xs

unlines' :: [String] -> String
unlines' [] = ""
unlines' (x:xs) = x ++ "\n" ++ unlines' xs

unwords' :: [String] -> String
unwords' [] = ""
unwords' (x:xs) = x ++ " " ++ unwords' xs

{-
The project from the previous chapter was implemented without using eta
reductions, to make the syntax slightly more digestible. Now that we know
how these reductions work, check the source code for the project and see if
you can perform any reductions!
-}

{-
A few reductions can be performed:

upperRot :: Int -> Char -> Char
upperRot = alphabetRot upperAlphabet

lowerRot :: Int -> Char -> Char
lowerRot = alphabetRot lowerAlphabet

caesar :: Int -> String -> String
caesar n = map (\ch -> rotChar n ch)

rot13 :: String -> String
rot13 = caesar 13
-}

{-
We discussed how we could extend the data type and function in listing 3.17
to create a center padding, meaning that the input string is being padded such
that it appears in the middle of the result string. Do this!
-}

{-
To solve this, we can extend the PadMode type and add a new case to the pattern
matching. We also add a new definition to our let binding of a halved padding
and add that to both sides of our string!

Luckily it's hard to forget the extension of the pattern matching since the
compiler warns us, if we miss it! Neat!
-}

data PadMode = PadLeft | PadRight | PadCenter

pad' :: PadMode -> Int -> String -> String
pad' mode n str =
  let diff = n - length str
      padding = replicate diff ' '
      halfPadding = replicate (diff `div` 2) ' '
   in case mode of
        PadLeft -> padding ++ str
        PadRight -> str ++ padding
        PadCenter -> halfPadding ++ str ++ halfPadding

{-
Just like `map`, `zip` is a typical bread and butter function that any Haskell
developer should know by heart. To get most familiar with it, implement this
function using a recursive algorithm! Then, extend the function to create
`zipWith`. Bonus question: Can you use `zipWith` to implement `zip`?
-}

{-
This function can be implemented quickly with clever use of pattern matching.
Remember that the first pattern that matches, is the one our program is going
with. So by providing cases for which any of the lists is empty and providing
an empty list as the result we can already quickly enforce the rule that the
shortest list of the two inputs will govern the total length of the resulting
list. In the case that both lists contain elements we are able to simply
build a tuple from them or combine them with the provided function in the
case of the zipWith function. zip can be constructed simply by providing a
function that builds tuples from two elements! This is a nice example of a
more powerful (higher-order) function being used to arrive at the definition
of a less powerful function with diverse application.
-}

zip' :: [a] -> [b] -> [(a,b)]
zip' [] [] = []
zip' _ [] = []
zip' [] _ = []
zip' (x:xs) (y:ys) = (x,y) : zip' xs ys

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] [] = []
zipWith' _ _ [] = []
zipWith' _ [] _ = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

zip'' :: [a] -> [b] -> [(a,b)]
zip'' = zipWith' (\x y -> (x,y))

{-
For simplicities sake we can assume the types for the two action map-functions
to look like this:

----
mapM :: (a -> IO b) -> [a] -> IO [b]

mapM_ :: (a -> IO b) -> [a] -> IO ()
----

Write these functions yourself! First, closely analyze how our pure `map`
function was built, then take a look at the `interactiveLines` example from the
beginning of this chapter. You can write `mapM` and `mapM_` the same way!
-}

{-
This exercise makes us rethink a few things. We are not evaluating functions
anymore but IO actions. This is a big difference. As an example, we can take
a look at the `sequence` function that takes a list of IO actions and results
in a single IO action that contains a list of the results gathered from the
IO actions.

We observe that this function evaluates to an IO action! So if we want to use
its result we have to evaluate it like any other IO action (inside the
do notation).

This holds true for our map functions. The main difference between them being
the `mapM_` variant to simply ignore the results. Note that there is no final
`return ()` since the last action evaluated in do notation is the final value
being returned / evaluated.
-}

sequence' :: [IO a] -> IO [a]
sequence' [] = return []
sequence' (act:xs) = do
  res <- act
  recRes <- sequence' xs
  return (res : recRes)

mapM' :: (a -> IO b) -> [a] -> IO [b]
mapM' _ [] = return []
mapM' f (x:xs) = do
  res <- f x
  recRes <- mapM' f xs
  return (res : recRes)

mapM_' :: (a -> IO b) -> [a] -> IO ()
mapM_' _ [] = return ()
mapM_' f (x:xs) = do
  f x
  mapM_' f xs