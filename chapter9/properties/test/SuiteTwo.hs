{-# LANGUAGE TemplateHaskell #-}

module SuiteTwo
  ( runTests,
  )
where

import Test.QuickCheck

prop_addPos :: Int -> Int -> Property
prop_addPos x y =
  withMaxSuccess 500 $
    x > 0 && y > 0 ==> x + y > 0

prop_multZero :: Int -> Property
prop_multZero x =
  noShrinking $
    cover 95 (x /= 0) "non-zero" $ x * 0 == 0

return [] -- See https://hackage.haskell.org/package/QuickCheck-2.14.2/docs/Test-QuickCheck-All.html#g:1

runTests :: IO Bool
runTests = $quickCheckAll
