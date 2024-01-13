{-# LANGUAGE TemplateHaskell #-}

module Main
  ( main,
  )
where

import Composition.Performance
import Composition.Pitch
import qualified Data.List as L
import Sound.Sound
import System.Exit (exitFailure, exitSuccess)
import Test.QuickCheck
import Util.Types

{-
So adding or subtracting 12 semitones from a given one should result in double or half
the frequency? That sounds a lot like a formal property! Write a QuickCheck test for
the frequency calculations of Semitone. What issue arises when constructing this
test? How can we fix it?
-}

{-
Naivly, we might implement the property like so:

prop_semitoneFreq :: Semitone -> Property
prop_semitoneFreq st =
  toFrequency st === toFrequency (st + 12) / 2
    .&. toFrequency st === toFrequency (st - 12) * 2

However, this will yield a peculiar result:

*** Failed! Falsified (after 23 tests):
-17
RHS
164.81377845643496 /= 164.813778456435

The test fails due to rounding errors! We fix this by
defining some epsilon which is the smallest difference
we allow and check if the values are in that range.

To get a nicer output use the `counterexample` function
from QuickChck to properly format a failing condition.
-}

(=~=) :: (Fractional a, Ord a, Show a) => a -> a -> Property
(=~=) x y =
  let epsilon = 0.00001
      prop = x >= y - epsilon && x <= y + epsilon
   in flip counterexample prop $ show x ++ " /= " ++ show y

infix 4 =~=

prop_semitoneFreq :: Semitone -> Property
prop_semitoneFreq st =
  toFrequency st =~= toFrequency (st + 12) / 2
    .&. toFrequency st =~= toFrequency (st - 12) * 2

{-
Create new QuickCheck properties for the new type by also implementing a
Arbitrary Chromatic instance and check if the conversion to Semitone as well
as Hz are correctly working.
-}

{-
The `Arbitrary Chromatic` instance can be found in the Composition.Pitch module.
-}

prop_chromaticSemitone :: Chromatic -> Property
prop_chromaticSemitone chromatic@(Chromatic name oct) =
  chromaticToSemitone chromatic === chromaticToSemitone (Chromatic name (oct + 1)) - 12
    .&. chromaticToSemitone chromatic === chromaticToSemitone (Chromatic name (oct - 1)) + 12

prop_chromaticFreq :: Chromatic -> Property
prop_chromaticFreq chromatic@(Chromatic name oct) =
  toFrequency chromatic =~= toFrequency (Chromatic name (oct + 1)) / 2
    .&. toFrequency chromatic =~= toFrequency (Chromatic name (oct - 1)) * 2

prop_consolidateSilence :: Performance -> Property
prop_consolidateSilence perf =
  let perf' = L.sortBy (\x y -> compare (start x) (start y)) perf
   in filter isTone perf' === filter isTone (consolidateSilence perf')

{-
As with many functions our `mix` and `overlaps` functions have some properties to
them. Especially `mix` is a critical function, due to the clipping issue explained earlier.
Write some QuickCheck properties that check the correctness of this function.
-}

{-
We implement a custom generator for signals and explicitly use it in our property with
`forAll`. With `listOf1` we make sure that there is atleast one signal. Otherwise the
usage of `maximum` would fail. Then we check that no clipping occurs and that the mixed
signal has the length of the longest signal.
-}

signalGen :: Gen Signal
signalGen = listOf $ choose (-1, 1)

prop_mix :: Property
prop_mix =
  forAll (listOf1 signalGen) $ \signals -> noClip signals && lengthProp signals
  where
    noClip signals = not $ any (\x -> x > 1 || x < (-1)) (mix signals)
    lengthProp signals = maximum (map L.length signals) == L.length (mix signals)

return [] -- See https://hackage.haskell.org/package/QuickCheck-2.14.2/docs/Test-QuickCheck-All.html#g:1

main :: IO ()
main = do
  res <- $quickCheckAll
  if res
    then exitSuccess
    else exitFailure
