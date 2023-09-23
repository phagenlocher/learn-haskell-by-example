module Csv.Parsing where

import Csv.Types
import qualified Data.Either as E
import qualified Data.List as L
import qualified Data.Maybe as M
import qualified Data.Text as T
import qualified Util.Text as UT

data Separators = Separators
  { sepLineSeparator :: Char,
    sepFieldSeparator :: Char
  }
  deriving (Eq, Show)

data HeaderOption = WithHeader | WithoutHeader
  deriving (Eq, Show)

data CsvParseOptions = CsvParseOptions
  { cpoSeparators :: Separators,
    cpoHeaderOption :: HeaderOption
  }
  deriving (Eq, Show)

defaultSeparators :: Separators
defaultSeparators =
  Separators
    { sepLineSeparator = '\n',
      sepFieldSeparator = ','
    }

defaultOptions :: CsvParseOptions
defaultOptions =
  CsvParseOptions
    { cpoSeparators = defaultSeparators,
      cpoHeaderOption = WithoutHeader
    }

parseWithHeader :: T.Text -> Either String Csv
parseWithHeader =
  parseCsv (defaultOptions {cpoHeaderOption = WithHeader})

parseWithoutHeader :: T.Text -> Either String Csv
parseWithoutHeader = parseCsv defaultOptions

parseCsv ::
  CsvParseOptions ->
  T.Text ->
  Either String Csv
parseCsv options raw = case lines of
  [] -> mkCsv Nothing []
  ((_, firstLine) : rest) ->
    let expectedLength = length $ splitFields firstLine
     in case cpoHeaderOption options of
          WithHeader ->
            let headerFields = splitFields firstLine
             in unsafeMkCsv (Just headerFields)
                  <$> parseColumns expectedLength rest
          WithoutHeader ->
            unsafeMkCsv Nothing <$> parseColumns expectedLength lines
  where
    lines :: [(Int, T.Text)]
    lines =
      L.filter (\(_, t) -> not $ T.null t) $
        L.zip [1 ..] $
          T.splitOn
            (T.singleton (sepLineSeparator $ cpoSeparators options))
            raw

    splitFields :: T.Text -> [T.Text]
    splitFields = L.map T.strip . T.splitOn separator
      where
        separator :: T.Text
        separator =
          T.singleton $
            sepFieldSeparator (cpoSeparators options)

    parseColumns ::
      Int ->
      [(Int, T.Text)] ->
      Either String [[DataField]]
    parseColumns expectedLength lines =
      let textColumns =
            L.transpose
              <$> L.foldl' parseRow (Right []) lines
       in fmap (L.map (L.map textToDataField)) textColumns
      where
        parseRow ::
          Either String [[T.Text]] ->
          (Int, T.Text) ->
          Either String [[T.Text]]
        parseRow mRows (lNum, line) =
          E.either
            Left
            ( \rows ->
                let fields = splitFields line
                 in if length fields /= expectedLength
                      then
                        Left $
                          "Number of fields in line "
                            <> show lNum
                            <> " does not match"
                            <> " expected length of "
                            <> show expectedLength
                            <> "! Actual length is "
                            <> show (length fields)
                            <> "!"
                      else Right $ rows ++ [fields]
            )
            mRows
