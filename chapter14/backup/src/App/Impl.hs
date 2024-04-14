module App.Impl
  ( runApp,
    mainApp,
  )
where

import App.Types
import Control.Exception (SomeException, catch)
import Control.Monad (forM_, when)
import Control.Monad.IO.Class
import Control.Monad.Trans.RWS.Strict
import qualified Data.Sequence as Seq
import qualified System.Directory as Dir
import qualified System.Directory.Safe as Dir
import System.Exit (ExitCode (ExitFailure), exitWith)
import System.FilePath ((</>))
import System.FileType (determineFileType)
import System.IO (hPutStrLn, stderr)

logSingleEntry :: LogEntry -> App ()
logSingleEntry x = tell $ Log (Seq.singleton x)

logFailure :: FailureEntry -> App ()
logFailure = logSingleEntry . Failure

logSuccess :: SuccessEntry -> App ()
logSuccess = logSingleEntry . Success

logSkipped :: SkipEntry -> App ()
logSkipped = logSingleEntry . Skipped

runApp :: App () -> Config -> IO Log
runApp app config = do
  (_, _, syncLog) <-
    runRWST app config mempty
      `catch` exceptionHandler
  return syncLog
  where
    exceptionHandler :: SomeException -> IO a
    exceptionHandler ex = do
      hPutStrLn stderr $
        "Syncing interrupted by exception: "
          ++ show ex
      exitWith $ ExitFailure 1

data FileDiff
  = Matching
  | ModificationTimeDiff
  | DestinationMissing
  deriving (Show, Eq)

checkFileDiff :: FilePath -> FilePath -> App FileDiff
checkFileDiff src dst = do
  dstExists <- liftIO $ Dir.doesFileExist dst
  if not dstExists
    then return DestinationMissing
    else do
      srcModTime <- liftIO $ Dir.getModificationTime src
      dstModTime <- liftIO $ Dir.getModificationTime dst
      if srcModTime == dstModTime
        then return Matching
        else return ModificationTimeDiff

checkFileType :: FilePath -> App Bool
checkFileType path = do
  mRequiredFileType <- asks configRequiredFileType
  case mRequiredFileType of
    Just requiredFileType -> do
      fileType <- liftIO $ determineFileType path
      return (requiredFileType == fileType)
    Nothing -> return True

syncFile :: FilePath -> FilePath -> App ()
syncFile src dst = do
  curFiles <- gets statsTransferredFiles
  mMaxFiles <- asks configMaxTransferredFiles
  when (maybe True (curFiles <) mMaxFiles) $ do
    fileTypeCorrect <- checkFileType src
    if not fileTypeCorrect
      then
        logSkipped $
          SkipEntry dst "Required filetype did not match"
      else do
        filesMatch <- checkFileDiff src dst
        if filesMatch == Matching
          then logSkipped $ SkipEntry dst "Files matched"
          else do
            mDidCopy <-
              liftIO $ Dir.safeCopyFileAndCreateDir src dst
            case mDidCopy of
              Left ex -> logFailure $ FailureEntry dst ex
              Right () -> do
                modify (modifyTransferredFiles (+ 1))
                logSuccess $ SuccessEntry src dst

traverseAndCopy :: FilePath -> App ()
traverseAndCopy pathSuffix = do
  fromPath <- asks configFromPath
  toPath <- asks configToPath
  mMaxRecDepth <- asks configMaxRecursionDepth
  curRecDepth <- gets statsRecursionDepth
  let path = fromPath </> pathSuffix
  when (maybe True (curRecDepth <=) mMaxRecDepth) $ do
    mFilesAndDirs <-
      liftIO $ Dir.safeListFilesAndDirectories path
    case mFilesAndDirs of
      Left ex -> logFailure $ FailureEntry path ex
      Right (files, dirs) -> do
        forM_ files $ \fileName ->
          syncFile
            (fromPath </> pathSuffix </> fileName)
            (toPath </> pathSuffix </> fileName)
        modify (modifyRecursionDepth (+ 1))
        forM_ dirs $ traverseAndCopy . (pathSuffix </>)
        modify (modifyRecursionDepth (\x -> x - 1))

mainApp :: App ()
mainApp = traverseAndCopy ""
