module Caesar.QuickCheckProperties where

import Caesar.Caesar
import qualified Data.List as L
import Test.QuickCheck

prop_rot13Symm :: String -> Bool
prop_rot13Symm s = s == rot13 (rot13 s)

prop_alphabetRotClosed :: Property
prop_alphabetRotClosed =
  forAll gen prop
  where
    prop :: (Alphabet, Int, Char) -> Bool
    prop (alphabet, n, c) =
      let c' = alphabetRot alphabet n c
       in c' `elem` alphabet

    gen :: Gen (Alphabet, Int, Char)
    gen = do
      size <- getSize
      alphabet <- arbitrary `suchThat` (not . null)
      n <- choose (-size, size)
      c <- elements alphabet
      return (L.nub alphabet, n, c)
