module Main (main) where

import App
import Control.Monad (forM_, when)
import qualified Data.List as L
import Options.Applicative
import System.FilePath
import System.FileType

data CliArgs = CliArgs
  { cliArgsConfig :: Config,
    cliArgsVerbose :: Bool
  }

cliArgsParser :: Parser CliArgs
cliArgsParser = CliArgs <$> config <*> isVerbose
  where
    config =
      Config
        <$> fromPath
        <*> toPath
        <*> onlyFileType
        <*> maxRecDepth
        <*> maxFiles
    fromPath =
      strOption $
        long "from-path"
          <> metavar "PATH"
          <> help "Path to sync files from"
    toPath =
      strOption $
        long "to-path"
          <> metavar "PATH"
          <> help "Path to sync files to"
    fileTypeMetaVar = L.intercalate "|" $ map show allFileTypes
    onlyFileType =
      optional $
        option auto $
          long "only-filetype"
            <> help "Only consider files of a certain filetype"
            <> metavar fileTypeMetaVar
    maxRecDepth =
      optional $
        option auto $
          long "max-rec-depth"
            <> help "Limit maximum recursive depth to traverse"
            <> metavar "NUMBER"
    maxFiles =
      optional $
        option auto $
          long "max-files"
            <> help "Limit number of files to sync"
            <> metavar "NUMBER"
    isVerbose =
      switch
        ( short 'v'
            <> long "verbose"
            <> help "Enable verbose output"
        )

argumentParser :: ParserInfo CliArgs
argumentParser =
  info
    (cliArgsParser <**> helper)
    ( fullDesc
        <> progDesc "A simple file synchronization tool"
    )

printLog :: Log -> IO ()
printLog (Log xs) = forM_ xs (putStrLn . prettyEntry)
  where
    prettyEntry (App.Success (SuccessEntry _ toPath)) =
      "[*] " ++ takeFileName toPath
    prettyEntry (App.Failure (FailureEntry path exc)) =
      "[!] " ++ takeFileName path ++ ": " ++ show exc
    prettyEntry (App.Skipped (SkipEntry path reason)) =
      "[s] " ++ takeFileName path ++ ": " ++ reason

main :: IO ()
main = do
  args <- execParser argumentParser
  result <- runApp mainApp $ cliArgsConfig args
  when (cliArgsVerbose args) $ printLog result
