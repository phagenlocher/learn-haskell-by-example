module Caesar.SimpleCheckProperties where

import Caesar.Caesar
import qualified Data.List as L
import Test.SimpleCheck

propRot13Symmetrical :: IO ()
propRot13Symmetrical =
  propertyTest
    (\s -> s == rot13 (rot13 s))
    asciiString'
    100

propAlphabetRotClosed :: IO ()
propAlphabetRotClosed = propertyTest prop gen 100
  where
    prop (alphabet, n, c) =
      let c' = alphabetRot alphabet n c
       in c' `elem` alphabet

    gen = RandomIO $ do
      alphabet <-
        runRandomIO $
          asciiString' `suchThat` (not . null)
      n <- runRandomIO $ elements [-100 .. 100]
      c <- runRandomIO $ elements alphabet
      return (L.nub alphabet, n, c)

propIsMiscIsDefault :: IO ()
propIsMiscIsDefault =
  propertyTest
    (\c -> isMisc c == not (isLower c || isUpper c || isDigit c))
    one
    10000
