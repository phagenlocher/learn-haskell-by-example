{-# LANGUAGE OverloadedStrings #-}

module Parser.Core where

import Control.Applicative
import Control.Monad
import Data.Foldable (asum)
import qualified Data.Text as T
import Text.Read
import Prelude hiding (takeWhile)

type ErrorMessage = String

type ParseResult a = Either ErrorMessage (T.Text, a)

newtype Parser a = Parser {runParser :: T.Text -> ParseResult a}

eitherResult :: ParseResult a -> Either ErrorMessage a
eitherResult = fmap snd

maybeResult :: ParseResult a -> Maybe a
maybeResult (Right x) = Just $ snd x
maybeResult _ = Nothing

modifyErrorMessage ::
  (ErrorMessage -> ErrorMessage) ->
  Parser a ->
  Parser a
modifyErrorMessage f p =
  Parser $ \t -> case runParser p t of
    Left msg -> Left $ f msg
    result -> result

char :: Parser Char
char =
  Parser $ \t ->
    case T.uncons t of
      Just (c, rest) -> Right (rest, c)
      Nothing -> Left "empty input"

string :: T.Text -> Parser T.Text
string str =
  Parser $ \t ->
    if T.take (T.length str) t == str
      then Right (T.drop (T.length str) t, str)
      else Left $ "failed to parse \"" ++ T.unpack str ++ "\""

takeWhile :: (Char -> Bool) -> Parser T.Text
takeWhile p = Parser $ \t -> Right (T.dropWhile p t, T.takeWhile p t)

spaces :: Parser T.Text
spaces = takeWhile (' ' ==)

someSpaces :: Parser T.Text
someSpaces = T.concat <$> some (string " ")

-- integer :: Parser Integer
-- integer = read . T.unpack <$> takeWhile (`elem` ['0' .. '9'])

integer :: Parser Integer
integer = do
  intStr <- takeWhile (`elem` ['0' .. '9'])
  case readMaybe (T.unpack intStr) of
    Just value -> return value
    Nothing ->
      fail $
        "Could not convert \""
          ++ T.unpack intStr
          ++ "\" as an integer"

choice :: [Parser a] -> Parser a
choice = asum

instance Functor Parser where
  fmap f p = Parser $ \t -> fmap (fmap f) (runParser p t)

class Functor f => Monoidal f where
  unit :: f ()
  (**) :: f a -> f b -> f (a, b)

instance Applicative Parser where
  pure x = Parser $ \t -> Right (t, x)
  (<*>) a b =
    Parser $ \t ->
      case runParser a t of
        Left msg -> Left msg
        Right (rest, f) -> runParser (fmap f b) rest

instance Alternative Parser where
  empty = Parser $ \_ -> Left "empty alternative"
  (<|>) a b =
    Parser $ \t ->
      case runParser a t of
        Left msg -> runParser (modErr msg b) t
        right -> right
    where
      modErr msg =
        modifyErrorMessage (\msg' -> msg ++ " and " ++ msg')

instance Monad Parser where
  return = pure
  (>>=) p f =
    Parser $ \t ->
      case runParser p t of
        Left msg -> Left msg
        Right (rest, x) -> runParser (f x) rest

instance MonadFail Parser where
  fail s = Parser $ \_ -> Left s

instance MonadPlus Parser where
  mzero = modifyErrorMessage (const "mzero") empty
