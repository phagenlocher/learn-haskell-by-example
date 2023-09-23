module Caesar where

import qualified Data.List as L
import Test.SimpleCheck

type Alphabet = [Char]

lowerAlphabet :: Alphabet
lowerAlphabet = ['a' .. 'z']

upperAlphabet :: Alphabet
upperAlphabet = ['A' .. 'Z']

digits :: Alphabet
digits = ['0' .. '9']

isLower :: Char -> Bool
isLower char = char `elem` lowerAlphabet

isUpper :: Char -> Bool
isUpper char = char `elem` upperAlphabet

isDigit :: Char -> Bool
isDigit char = char `elem` digits

isMisc :: Char -> Bool
isMisc char = char `notElem` lowerAlphabet ++ upperAlphabet ++ digits

indexOf :: Char -> Alphabet -> Int
indexOf ch [] = undefined
indexOf ch (x : xs) = if x == ch then 0 else 1 + indexOf ch xs

alphabetRot :: Alphabet -> Int -> Char -> Char
alphabetRot alphabet n ch =
  alphabet !! ((indexOf ch alphabet + n) `mod` length alphabet)

upperRot :: Int -> Char -> Char
upperRot = alphabetRot upperAlphabet

lowerRot :: Int -> Char -> Char
lowerRot = alphabetRot lowerAlphabet

rotChar :: Int -> Char -> Char
rotChar n ch
  | isLower ch = lowerRot n ch
  | isUpper ch = upperRot n ch
  | otherwise = ch

caesar :: Int -> String -> String
caesar n = map $ rotChar n

rot13 :: String -> String
rot13 = caesar 13

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
