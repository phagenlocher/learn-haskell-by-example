module Util.Arguments
  ( getBool,
    getChar,
    getText,
    getInterval,
  )
where

import qualified Csv.Parsing as CP
import qualified Data.List as L
import qualified Data.Maybe as M
import qualified Data.Text as T
import System.Environment (getArgs)
import Text.Read (readMaybe)
import qualified Util.Text as UT
import Prelude hiding (getChar)

getArguments :: IO [T.Text]
getArguments = map T.pack <$> getArgs

getValueOf :: T.Text -> IO (Maybe T.Text)
getValueOf key = do
  L.foldl
    ( \mVal arg ->
        if M.isNothing mVal
          then T.stripPrefix argKey arg
          else mVal
    )
    Nothing
    <$> getArguments
  where
    argKey = "--" <> key <> "="

getBool :: T.Text -> IO Bool
getBool key =
  L.elem argKey <$> getArguments
  where
    argKey = "--" <> key

getChar :: T.Text -> IO (Maybe Char)
getChar key = do
  sep <- getValueOf key
  return $ case T.uncons <$> sep of
    Just (Just (c, rest)) ->
      if T.null rest
        then Just c
        else Nothing
    _ -> Nothing

getText :: T.Text -> IO (Maybe T.Text)
getText = getValueOf

getInterval :: T.Text -> IO (Maybe (Int, Int))
getInterval key = do
  mVal <- fmap T.strip <$> getValueOf key
  case mVal of
    Nothing -> return Nothing
    Just val ->
      let (a, b) = T.breakOn "," val
       in case (readMaybe $ T.unpack a, readMaybe $ T.unpack (T.tail b)) of
            (Just x, Just y) -> return $ Just (x, y) -- Parsed as Ints correctly
            _ -> return Nothing
