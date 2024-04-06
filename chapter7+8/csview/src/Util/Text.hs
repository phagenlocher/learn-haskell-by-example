module Util.Text
  ( startsWith,
    endsWith,
    isEnclosedBy,
    quote,
    contains,
  )
where

import qualified Data.Text as T

startsWith :: T.Text -> Char -> Bool
startsWith t c =
  case T.uncons t of
    Just (hd, _) -> c == hd
    _ -> False

endsWith :: T.Text -> Char -> Bool
endsWith t c =
  case T.unsnoc t of
    Just (_, tl) -> c == tl
    _ -> False

isEnclosedBy :: T.Text -> Char -> Bool
isEnclosedBy t c = t `startsWith` c && t `endsWith` c

quote :: T.Text -> T.Text
quote = T.cons '"' . flip T.snoc '"' . T.replace "\"" "\"\"\""

contains :: T.Text -> T.Text -> Bool
contains = flip T.isInfixOf
