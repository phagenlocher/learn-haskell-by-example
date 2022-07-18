module Exercises where

import Lib -- Import everything from Lib
import Data.Char (toLower)
import Data.List (sortBy, find)

{-
Try to write this `(!!)` function yourself using pattern matching and a
recursive approach. In the case that the index is too large or negative
you can use `undefined` to make your function throw an exception.
-}

{-
This index function needs to find the element in a list at a certain index.
If this index is 0, the current head of the list is the wanted element. So,
if the list is empty, we cannot we fail, but if it is non empty and the index
is larger than zero the wanted element is somewhere in the tail of the list.
Thus, the function calls itself recursively on the tail while decrementing the
wanted index (since we skipped on element in the original list).
-}
index [] _ = undefined
index (x:xs) n =
  if n < 0
    then undefined
    else if n == 0
      then x
      else index xs (n-1)

-- This alternative solution uses guards instead of the if-clause
index' [] _ = undefined
index' (x:xs) n
  | n < 0 = undefined
  | n == 0 = x
  | otherwise = index xs (n-1)

{-
ROT13 is nicely symmetrical, however we needed to omit the rotation of digits
because that would have made it unsymmetrical, since sizes for the alphabets
are different. However, the rotation by an offset of 5 makes the rotation for
digits symmetrical again! Your task is to combine ROT13 and ROT5 (for digits)
to a combined ROT13.5 to have a symmetrical encoding for Latin letters and
digits!

Write a function `rot135` that performs this encoding!

Make sure that two applications of this function cancel each other out:

----
ghci> rot135 "Hello 1 2 3 ... 7 8 9"
"Uryyb 6 7 8 ... 2 3 4"
ghci> rot135 (rot135 "Hello 1 2 3 ... 7 8 9")
"Hello 1 2 3 ... 7 8 9"
----
-}

{-
This solution is achieved by recursively taking apart a string and putting
it back together. Each character gets rotated differently based on what
kind of character we encounter. For letters from the Latin alphabet we perform
similar rotations as our `rotChar` function. Here, the rotation offset of 13
is fixed. Additionally, if the character is a digit we rotate it in the
alphabet of digits by an offset of 5. If we deal with some miscellaneous
character we simply keep it unchanged and continue the computation recursively.
-}
rot135 :: String -> String
rot135 [] = []
rot135 (ch : xs)
  | isLower ch = lowerRot 13 ch : rot135 xs
  | isUpper ch = upperRot 13 ch : rot135 xs
  | isDigit ch = alphabetRot digits 5 ch : rot135 xs
  | otherwise = ch : rot135 xs


{-
ROT13 and the Caesars cipher is not a safe way to "encrypt" a message. This can
easily shown by frequency analysis. In the English language some letters are
simply more common than other ones (e.g. "e" is much more common then other
letters). Write a function that counts the occurrences of a character within a
string.

----
count :: Char -> String -> Int`
----

Then use this function to perform a frequency analysis on a plain text and an
"encrypted text". In order to do this, you apply the counting function to every
letter in the alphabets we have defined earlier. Is there a way of writing a
"best-guess" decryption scheme for the Caesars cipher?
-}

{-
The first part of this exercise is writing the `count` function. Like the other
exercises so far, its a recursive function using pattern matching on lists
(can you already sense a pattern here?). In the case of the empty list, the
result is trivial. However, if the list contains some element we can check if
the head equals the character we search for. If yes, we continue counting
recursively but add a one to the recursive result (since we found the
character). Otherwise, we don't add anything. Observe how the case of the
empty list serves as the condition for which this recursion stops.
-}
count :: Char -> String -> Int
count elem [] = 0
count elem (x : xs)
  | x == elem = 1 + count elem xs
  | otherwise = count elem xs

{-
The second part of this exercise covers some syntax which is explained in
chapter 3, so only the rough idea will be sketched out here. The code contains
some comments but don't worry if you don't understand them yet!

While we could count each occurrence of each letter by ourselves we construct
a function that creates a list of tuples of each char and how often it occurs.

----
ghci> frequencyStats loremIpsum
[('e',56),('t',50),('a',46),('o',42),('s',38),('m',32),('r',32),('i',30),
('u',30),('d',26),('l',22),('n',20),('c',12),('p',10),('g',8),('b',6),('v',6),
('k',4),('y',4),('j',2),('q',2),('f',0),('h',0),('w',0),('x',0),('z',0)]
ghci> frequencyStats (rot13 loremIpsum)
[('r',56),('g',50),('n',46),('b',42),('f',38),('e',32),('z',32),('h',30),
('v',30),('q',26),('y',22),('a',20),('p',12),('c',10),('t',8),('i',6),('o',6),
('l',4),('x',4),('d',2),('w',2),('j',0),('k',0),('m',0),('s',0),('u',0)]
----

What we can obverse is that the frequency at which letters appear in a string
is unaltered by Caesars cipher! So if we simply look for the most common letter
in the encrypted message and calculate its distance to the letter 'e' we have a
good guess for the encryption!

----
ghci> tryToDecrypt (rot13 loremIpsum)
"Lorem ipsum dolor sit amet, consetetur sadipscing elitr, sed diam nonumy
eirmod tempor invidunt ut labore et dolore magna aliquyam erat, sed diam
voluptua. At vero eos et accusam et justo duo dolores et ea rebum. Stet clita
kasd gubergren, no sea takimata sanctus est Lorem ipsum dolor sit amet. Lorem
ipsum dolor sit amet, consetetur sadipscing elitr, sed diam nonumy eirmod
tempor invidunt ut labore et dolore magna aliquyam erat, sed diam voluptua. At
vero eos et accusam et justo duo dolores et ea rebum. Stet clita kasd
gubergren, no sea takimata sanctus est Lorem ipsum dolor sit amet."
-}
frequencyStats :: [Char] -> [(Char, Int)]
frequencyStats xs =
  let input = map toLower xs -- Transform the input to lowercase
      freqs = -- Map each character with how often it appears in the input
        map (\elem -> (elem, count elem input)) lowerAlphabet
   -- Here we now sort the tuples of characters and how often they occur
   in sortBy (\(_, x) (_, y) -> compare y x) freqs

tryToDecrypt :: String -> String
tryToDecrypt "" = ""
tryToDecrypt msg =
  let
    -- The head of the resulting list from frequencyStats contains the most
    -- frequenct letter, everything else is ignored. We perform a pattern
    -- match of which we are certain it will work!
    ((mostCommonLetter,_):_) = frequencyStats msg
    -- An associative list (see chapter 3) containing letters and their
    -- distance to the letter 'e'. Check for yourself that this is correct!
    distances = zip ['a'..'z'] [-4,-3..]
    -- We look up the distance of the most common letter to 'e' by performing
    -- a lookup on the associative list containing the distances. We receive
    -- a Maybe of the wanted element and pattern match it accordingly.
    -- (Please see chapter 3 for this!)
    Just guessedDistance = lookup mostCommonLetter distances
   -- Now we can finally try to decrypt our message by performing the cipher
   -- with the guessed distance!
   in caesar guessedDistance msg

