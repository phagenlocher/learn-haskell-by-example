{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module App.Types
  ( App,
    Config (..),
    Stats (..),
    modifyRecursionDepth,
    modifyTransferredFiles,
    Log (..),
    LogEntry (..),
    SuccessEntry (..),
    FailureEntry (..),
    SkipEntry (..),
  )
where

import Control.Exception (IOException)
import Control.Monad.Trans.RWS.Strict (RWST)
import qualified Data.Sequence as Seq
import System.FileType (FileType)

data Config = Config
  { configFromPath :: FilePath,
    configToPath :: FilePath,
    configRequiredFileType :: Maybe FileType,
    configMaxRecursionDepth :: Maybe Int,
    configMaxTransferredFiles :: Maybe Int
  }
  deriving (Show, Eq)

data Stats = Stats
  { statsRecursionDepth :: Int,
    statsTransferredFiles :: Int
  }
  deriving (Show, Eq)

modifyRecursionDepth :: (Int -> Int) -> Stats -> Stats
modifyRecursionDepth f (Stats recDepth files) =
  Stats (f recDepth) files

modifyTransferredFiles :: (Int -> Int) -> Stats -> Stats
modifyTransferredFiles f (Stats recDepth files) =
  Stats recDepth (f files)

instance Semigroup Stats where
  a <> b =
    Stats
      { statsRecursionDepth = max `on` statsRecursionDepth,
        statsTransferredFiles = (+) `on` statsTransferredFiles
      }
    where
      f `on` field = f (field a) (field b)

instance Monoid Stats where
  mappend = (<>)
  mempty =
    Stats
      { statsRecursionDepth = 0,
        statsTransferredFiles = 0
      }

data SuccessEntry = SuccessEntry
  { successFromPath :: FilePath,
    successToPath :: FilePath
  }
  deriving (Show, Eq)

data FailureEntry = FailureEntry
  { failureFilePath :: FilePath,
    failureException :: IOException
  }
  deriving (Show, Eq)

data SkipEntry = SkipEntry
  { skipFilePath :: FilePath,
    skipReason :: String
  }
  deriving (Show, Eq)

data LogEntry
  = Success SuccessEntry
  | Failure FailureEntry
  | Skipped SkipEntry
  deriving (Show, Eq)

newtype Log = Log (Seq.Seq LogEntry)
  deriving stock (Show, Eq)
  deriving newtype (Semigroup, Monoid)

type App a = RWST Config Log Stats IO a
