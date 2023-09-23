{-# LANGUAGE ViewPatterns #-}

module Csv.Print where

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

writeCsvToHandle :: H.Handle -> Csv -> IO ()
writeCsvToHandle handle csv =
  mapM_ (TIO.hPutStrLn handle) $ toFileContent csv

writeCsv :: FilePath -> Csv -> IO ()
writeCsv path csv = do
  handle <- FD.openFile path SIO.WriteMode
  writeCsvToHandle handle csv
  H.hClose handle

printCsv :: Csv -> IO ()
printCsv = writeCsvToHandle SIO.stdout

data PrettyCsv = PrettyCsv
  { pc_header :: Maybe [T.Text],
    pc_columns :: [[T.Text]],
    pc_summaries :: Maybe [T.Text]
  }
  deriving (Eq, Show)

fromCsv :: Csv -> PrettyCsv
fromCsv Csv {..} =
  PrettyCsv
    { pc_header = csvHeader,
      pc_columns =
        map (map dataFieldToText) csvColumns,
      pc_summaries = Nothing
    }

unsafeWithSummaries :: PrettyCsv -> [T.Text] -> PrettyCsv
unsafeWithSummaries pcsv summaries =
  E.either error id $ withSummaries pcsv summaries

withSummaries ::
  PrettyCsv ->
  [T.Text] ->
  Either String PrettyCsv
withSummaries pcsv@(PrettyCsv {..}) summaries
  | length summaries /= length pc_columns =
    Left "The number of summaries does not match the number of columns"
  | otherwise = Right $ pcsv {pc_summaries = Just $ summaries}

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
        if M.isJust pc_header
          then [outer, header, inner, addSummary rows, outer]
          else [outer, addSummary allRows, outer]
      where
        addSummary [] = ""
        addSummary rows =
          T.intercalate "\n" $
            if M.isJust pc_summaries
              then (L.init rows) ++ [inner, L.last rows]
              else rows

    allColumns :: [[T.Text]]
    allColumns = case (pc_header, pc_summaries) of
      (Nothing, Nothing) ->
        pc_columns
      (Just header, Nothing) ->
        L.zipWith (:) header pc_columns
      (Nothing, Just summaries) ->
        L.zipWith (\c s -> c ++ [s]) pc_columns summaries
      (Just header, Just summaries) ->
        L.zipWith3 (\h c s -> h : c ++ [s]) header pc_columns summaries

    prettyRow :: T.Text -> [T.Text] -> T.Text
    prettyRow delimiter =
      L.foldl' (\acc x -> T.concat [acc, x, delimiter]) delimiter

    padField :: Int -> T.Text -> T.Text
    padField n field = T.concat [" ", field, T.replicate n " ", " "]

    fieldPaddings :: [T.Text] -> [Int]
    fieldPaddings col =
      map (\x -> maximum (L.map T.length col) - T.length x) col
