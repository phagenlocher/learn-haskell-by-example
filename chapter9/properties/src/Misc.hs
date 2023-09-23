module Misc where

import Data.List
import Test.QuickCheck
import Test.SimpleCheck

prop_sortSorts :: [Int] -> Property
prop_sortSorts xs =
  collect (null xs) $ sort `sorts` xs
