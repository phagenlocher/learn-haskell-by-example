module Test.SimpleCheck where

import Data.Char
import qualified Data.List as L
import System.Random (Random, random, uniformR)
import System.Random.Stateful (StdGen, applyAtomicGen, getStdGen, globalStdGen, setStdGen)

newtype RandomState = RandomState Int deriving (Eq, Show)

randomInt :: RandomState -> (Int, RandomState)
randomInt (RandomState rs) = (newRs, RandomState newRs)
  where
    newRs = (1103515245 * rs + 12345) `mod` (2 ^ 31)

randomIntList :: RandomState -> Int -> [Int]
randomIntList rs n
  | n <= 0 = []
  | otherwise =
    let (v, rs') = randomInt rs
     in v : randomIntList rs' (n - 1)

randomListN :: (Random a) => StdGen -> Int -> ([a], StdGen)
randomListN gen n
  | n <= 0 = ([], gen)
  | otherwise =
    let (v, gen') = random gen
        (xs, gen'') = randomListN gen' (n - 1)
     in (v : xs, gen'')

randomList :: (Random a) => StdGen -> Int -> ([a], StdGen)
randomList gen max = randomListN gen' n
  where
    (n, gen') = uniformR (0, max) gen

randomList' :: (Random a) => StdGen -> ([a], StdGen)
randomList' = flip randomList 100

randomListIO :: (Random a) => IO [a]
randomListIO = do
  g <- getStdGen
  let (xs, g') = randomList' g
  setStdGen g'
  return xs

applyGlobalStdGen :: (StdGen -> (a, StdGen)) -> IO a
applyGlobalStdGen f = applyAtomicGen f globalStdGen

sorted :: Ord a => [a] -> Bool
sorted [] = True
sorted [x] = True
sorted (x : y : xs) = x <= y && sorted (y : xs)

sorts :: Ord a => ([a] -> [a]) -> [a] -> Bool
sorts f input = sorted $ f input

propertyTestSorts :: ([Int] -> [Int]) -> Int -> IO ()
propertyTestSorts f n
  | n <= 0 = putStrLn "Test successful!"
  | otherwise = do
    xs <- applyGlobalStdGen randomList'
    if f `sorts` xs
      then propertyTestSorts f $ n - 1
      else putStrLn $ "Test failed on: " <> show xs

newtype RandomIO a = RandomIO {runRandomIO :: IO a}

{-
suchThat acts more or less like a filter on our random generator. What we have
not yet build is an equivalent to the map function for our random values.
To keep things as general as possible we might want to implement an
instance for the `Functor` type class for `RandomIO` where `fmap` applies some
function to the random value. Implement this instance!
-}

instance Functor RandomIO where
  fmap f rio = RandomIO $ fmap f (runRandomIO rio)

one :: Random a => RandomIO a
one = RandomIO $ applyGlobalStdGen random

some :: Random a => RandomIO [a]
some = RandomIO $ do
  n <- applyGlobalStdGen $ uniformR (0, 100)
  replicateIO n $ runRandomIO one

replicateIO :: Int -> IO a -> IO [a]
replicateIO n act
  | n <= 0 = return []
  | otherwise = do
    x <- act
    xs <- replicateIO (n - 1) act
    return $ x : xs

suchThat :: RandomIO a -> (a -> Bool) -> RandomIO a
suchThat rand pred = RandomIO $ do
  val <- runRandomIO rand
  if pred val
    then return val
    else runRandomIO $ suchThat rand pred

nonNegative :: (Num a, Ord a, Random a) => RandomIO a
nonNegative = one `suchThat` (> 0)

nonEmpty :: Random a => RandomIO [a]
nonEmpty = some `suchThat` (not . null)

asciiChar :: RandomIO Char
asciiChar = one `suchThat` (\c -> isAscii c && not (isControl c))

letterChar :: RandomIO Char
letterChar = asciiChar `suchThat` isLetter

manyOf :: RandomIO a -> RandomIO [a]
manyOf rio = RandomIO $ do
  n <- applyGlobalStdGen $ uniformR (0, 100)
  replicateIO n $ runRandomIO rio

asciiString :: RandomIO String
asciiString = manyOf asciiChar

letterString :: RandomIO String
letterString = manyOf letterChar

{-
The way we have defined `letterString` is highly inefficient. Instead of randomly
choosing from a set of wanted Char, we rather choose from a huge set which we then
filter by invalidating elements, we do not want to see. To make it a bit more efficient
and also more general write a function that receives a non-empty list of elements an
randomly chooses one. Then, use this function to build new ASCII and letter
generators that are faster this time.
-}

elements :: [a] -> RandomIO a
elements [] = error "elements cannot work with an empty list!"
elements xs = RandomIO $ do
  i <- applyGlobalStdGen $ uniformR (0, length xs - 1)
  return $ xs !! i

asciiChar' :: RandomIO Char
asciiChar' = elements ['\x20' .. '\x7e'] -- ASCII codes given directly as hex-values; see https://theasciicode.com.ar

letterChar' :: RandomIO Char
letterChar' =
  elements "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"

asciiString' :: RandomIO String
asciiString' = manyOf asciiChar'

letterString' :: RandomIO String
letterString' = manyOf letterChar'

{-
We have already implemented `suchThat` in our SimpleCheck framework. Now it is
time to copy more of the functions from QuickCheck to get a better feel for how it
computes the value it uses. Make sure that the type signature is the same (just using
RandomIO instead of Gen) and try to keep the performance as good as it can be.
-}

oneof :: [RandomIO a] -> RandomIO a
oneof rs = RandomIO $ do
  rio <- runRandomIO $ elements rs
  runRandomIO rio

listOf :: RandomIO a -> RandomIO [a]
listOf rio = RandomIO $ do
  n <- applyGlobalStdGen $ uniformR (0, 10000)
  runRandomIO $ vectorOf n rio

listOf1 :: RandomIO a -> RandomIO [a]
listOf1 rio = listOf rio `suchThat` (not . null)

vectorOf :: Int -> RandomIO a -> RandomIO [a]
vectorOf n rio = RandomIO $ replicateIO n (runRandomIO rio)

{-
NOTE: `shuffle` and `sublistOf` could have been implemented without (Eq a) by working
with the indices of the elements in the list
-}

shuffle :: (Eq a) => [a] -> RandomIO [a]
shuffle [] = RandomIO $ return []
shuffle xs = RandomIO $ do
  x <- runRandomIO $ elements xs
  xs' <- runRandomIO $ shuffle (L.delete x xs)
  return $ x : xs'

sublistOf :: (Eq a) => [a] -> RandomIO [a]
sublistOf [] = RandomIO $ return []
sublistOf (x : xs) = RandomIO $ do
  b <- runRandomIO (one :: RandomIO Bool)
  xs' <- runRandomIO $ sublistOf xs
  if b
    then return $ x : xs'
    else return xs'

propertyTest :: Show a => (a -> Bool) -> RandomIO a -> Int -> IO ()
propertyTest predicate random n
  | n <= 0 = putStrLn "Tests successful!"
  | otherwise = do
    testCase <- runRandomIO random
    if predicate testCase
      then propertyTest predicate random $ n - 1
      else putStrLn $ "Test failed on: " <> show testCase

propertyTestWithPreCondition ::
  Show a => (a -> Bool) -> (a -> Bool) -> RandomIO a -> Int -> IO ()
propertyTestWithPreCondition precond predicate random n
  | n <= 0 = putStrLn "Tests successful!"
  | otherwise = do
    testCase <- runRandomIO random
    if not (precond testCase)
      then propertyTestWithPreCondition precond predicate random n
      else
        if predicate testCase
          then propertyTestWithPreCondition precond predicate random $ n - 1
          else putStrLn $ "Test failed on: " <> show testCase

primes :: [Int]
primes = aux 2 [3 ..]
  where
    aux n ns =
      let (x : xs) = filter (\x -> x `mod` n /= 0) ns
       in n : aux x xs

prime :: Int -> Int
prime n = primes !! max 0 n

piFun :: Int -> Int
piFun n = length $ takeWhile (<= n) primes

adjPairs :: [a] -> [(a, a)]
adjPairs xs = zip xs (tail xs)

runLengthEncoding :: Eq a => [a] -> [(Int, a)]
runLengthEncoding [] = []
runLengthEncoding [x] = [(1, x)]
runLengthEncoding (x : y : xs)
  | x /= y = (1, x) : runLengthEncoding (y : xs)
  | otherwise = case runLengthEncoding (y : xs) of
    [] -> [(2, x)]
    (n, x) : rest -> (n + 1, x) : rest

runLengthDecoding :: [(Int, a)] -> [a]
runLengthDecoding = foldl (\acc (n, x) -> acc ++ replicate n x) []

primeRepeats :: [Int]
primeRepeats = map fst $ runLengthEncoding $ map piFun [2 ..]
