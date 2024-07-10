{-# LANGUAGE ScopedTypeVariables #-}

module System.Directory.Safe
  ( safeListFilesAndDirectories,
    safeCopyFileAndCreateDir,
  )
where

import Control.Exception
import Control.Monad (filterM)
import qualified System.Directory as Dir
import System.FilePath (takeDirectory, (</>))

safeListFilesAndDirectories ::
  FilePath -> IO (Either IOException ([FilePath], [FilePath]))
safeListFilesAndDirectories = try . listFilesAndDirectories

listFilesAndDirectories :: FilePath -> IO ([FilePath], [FilePath])
listFilesAndDirectories pathPrefix = do
  paths <- Dir.listDirectory pathPrefix
  files <-
    filterM
      (Dir.doesFileExist . (pathPrefix </>))
      paths
  dirs <-
    filterM
      (Dir.doesDirectoryExist . (pathPrefix </>))
      paths
  return (files, dirs)

listFilesRecursively :: FilePath -> IO [FilePath]
listFilesRecursively path = do
  (files, dirs) <- listFilesAndDirectories path
  files' <- concatMapM listFilesRecursively (map (path </>) dirs)
  return $ map (path </>) files ++ files'
  where
    -- This function is also provided by `Control.Monad.Extra` from
    -- the `extra` package
    concatMapM :: (Monad m) => (a -> m [b]) -> [a] -> m [b]
    concatMapM _ [] = return []
    concatMapM act (x : xs) = do
      xs' <- act x
      xs'' <- concatMapM act xs
      return $ xs' ++ xs''

safeCopyFileAndCreateDir ::
  FilePath -> FilePath -> IO (Either IOException ())
safeCopyFileAndCreateDir src dst = try $ do
  -- Exercise delete dir too!
  Dir.createDirectoryIfMissing True $ takeDirectory dst
  catchJust
    ( \(ex :: AsyncException) -> case ex of
        UserInterrupt -> Just ()
        _ -> Nothing
    )
    (Dir.copyFileWithMetadata src dst)
    (const $ Dir.removeFile dst >> throwIO UserInterrupt)
