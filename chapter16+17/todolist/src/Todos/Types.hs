{-# LANGUAGE DeriveGeneric #-}

module Todos.Types
  ( TodoEntry (..),
    sortTodoEntries,
  )
where

import qualified Data.Aeson as J
import qualified Data.List as L
import qualified Data.Maybe as M
import qualified Data.Text as T
import qualified Database.SQLite.Simple.FromRow as SQL
import qualified Database.SQLite.Simple.ToRow as SQL
import GHC.Generics (Generic)

data TodoEntry = TodoEntry
  { entryId :: Maybe Int,
    text :: T.Text,
    checked :: Bool,
    rank :: Maybe Int
  }
  deriving (Eq, Show, Generic)

instance J.ToJSON TodoEntry

instance J.FromJSON TodoEntry

instance SQL.FromRow TodoEntry

instance SQL.ToRow TodoEntry

sortTodoEntries :: [TodoEntry] -> [TodoEntry]
sortTodoEntries entries = L.sortOn rank ranked ++ unranked
  where
    (ranked, unranked) = L.partition (M.isJust . rank) entries
