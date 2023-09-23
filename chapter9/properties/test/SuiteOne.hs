{-# LANGUAGE TemplateHaskell #-}

module SuiteOne
  ( runTests,
  )
where

import Test.QuickCheck

prop_true :: Int -> Bool
prop_true = const True

prop_false :: Int -> Bool
prop_false = const False

return [] -- See https://hackage.haskell.org/package/QuickCheck-2.14.2/docs/Test-QuickCheck-All.html#g:1

runTests :: IO Bool
runTests = $quickCheckAll
