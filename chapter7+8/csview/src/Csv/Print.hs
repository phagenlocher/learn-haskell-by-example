{-# LANGUAGE ViewPatterns #-}

module Csv.Print
  ( writeCsv,
    printCsv,
    PrettyCsv (..),
    fromCsv,
    unsafeWithSummaries,
    withSummaries,
    pretty,
    prettyText,
  )
where

import Csv.Types
import qualified Data.Either as E
import qualified Data.List as L
import qualified Data.Maybe as M
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified GHC.IO.Handle as H
import qualified GHC.IO.Handle.FD as FD
import qualified System.IO as SIO
import qualified Util.Text as UT

quoteForFileContent :: T.Text -> T.Text
quoteForFileContent (T.strip -> t)
  | '"' `T.elem` t || ',' `T.elem` t = quoted
  | otherwise = t
  where
    quoted = T.concat ["\"", UT.quote t, "\""]

toFileContent :: Csv -> [T.Text]
toFileContent Csv {..} =
  let rows = L.map (L.map dataFieldToText) $ L.transpose csvColumns
   in L.map (T.intercalate ",") $
        M.maybe rows (: rows) csvHeader

writeCsv :: FilePath -> Csv -> IO ()
writeCsv path = TIO.writeFile path . T.intercalate "\n" . toFileContent

printCsv :: Csv -> IO ()
printCsv = mapM_ TIO.putStrLn . toFileContent

data PrettyCsv = PrettyCsv
  { pcHeader :: Maybe [T.Text],
    pcColumns :: [[T.Text]],
    pcSummaries :: Maybe [T.Text]
  }
  deriving (Eq, Show)

fromCsv :: Csv -> PrettyCsv
fromCsv Csv {..} =
  PrettyCsv
    { pcHeader = csvHeader,
      pcColumns =
        map (map dataFieldToText) csvColumns,
      pcSummaries = Nothing
    }

unsafeWithSummaries :: PrettyCsv -> [T.Text] -> PrettyCsv
unsafeWithSummaries pcsv summaries =
  E.either error id $ withSummaries pcsv summaries

withSummaries ::
  PrettyCsv ->
  [T.Text] ->
  Either String PrettyCsv
withSummaries pcsv@(PrettyCsv {..}) summaries
  | length summaries /= length pcColumns =
      Left "The number of summaries does not match the number of columns"
  | otherwise = Right $ pcsv {pcSummaries = Just $ summaries}

pretty :: PrettyCsv -> String
pretty = T.unpack . prettyText

prettyText :: PrettyCsv -> T.Text
prettyText PrettyCsv {..} =
  let paddings = map fieldPaddings allColumns
      columns = zipWith zip paddings allColumns
      padded = map (map $ uncurry padField) columns
      rows = L.transpose padded
      columnSizes = map (maximum . map T.length) padded
      prettyRows = map (prettyRow "|") rows
      outerBorder =
        flip T.replicate "-" $
          M.maybe 0 T.length (M.listToMaybe prettyRows)
      innerBorder = prettyRow "+" $ map (`T.replicate` "-") columnSizes
   in printRows (outerBorder, innerBorder) prettyRows
  where
    printRows :: (T.Text, T.Text) -> [T.Text] -> T.Text
    printRows (outer, inner) [] = ""
    printRows (outer, inner) allRows@(header : rows) =
      T.intercalate "\n" $
        if M.isJust pcHeader
          then [outer, header, inner, addSummary rows, outer]
          else [outer, addSummary allRows, outer]
      where
        addSummary [] = ""
        addSummary rows =
          T.intercalate "\n" $
            if M.isJust pcSummaries
              then (L.init rows) ++ [inner, L.last rows]
              else rows

    allColumns :: [[T.Text]]
    allColumns = case (pcHeader, pcSummaries) of
      (Nothing, Nothing) ->
        pcColumns
      (Just header, Nothing) ->
        L.zipWith (:) header pcColumns
      (Nothing, Just summaries) ->
        L.zipWith (\c s -> c ++ [s]) pcColumns summaries
      (Just header, Just summaries) ->
        L.zipWith3 (\h c s -> h : c ++ [s]) header pcColumns summaries

    prettyRow :: T.Text -> [T.Text] -> T.Text
    prettyRow delimiter =
      L.foldl' (\acc x -> T.concat [acc, x, delimiter]) delimiter

    padField :: Int -> T.Text -> T.Text
    padField n field = T.concat [" ", field, T.replicate n " ", " "]

    fieldPaddings :: [T.Text] -> [Int]
    fieldPaddings col =
      map (\x -> maximum (L.map T.length col) - T.length x) col
