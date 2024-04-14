{-# LANGUAGE OverloadedStrings #-}

module Todos.Database
  ( module Database.SQLite.Simple,
    setupDb,
    getEntries,
    addEntry,
    getEntryById,
    updateEntry,
    deleteEntry,
  )
where

import Control.Exception (try)
import Database.SQLite.Simple
import Todos.Types

setupDb :: Connection -> IO ()
setupDb dbConn =
  execute_
    dbConn
    "CREATE TABLE IF NOT EXISTS entries \
    \ (id INTEGER PRIMARY KEY AUTOINCREMENT, \
    \ text TEXT, checked INTEGER, rank INTEGER)"

getEntries :: Connection -> IO [TodoEntry]
getEntries dbConn = query_ dbConn "SELECT * FROM entries"

addEntry :: Connection -> TodoEntry -> IO (Either SQLError Int)
addEntry dbConn entry = try $
  withTransaction dbConn $ do
    execute
      dbConn
      "INSERT INTO entries (id, text, checked, rank) \
      \ VALUES (?,?,?,?)"
      entry
    newIdx <- lastInsertRowId dbConn
    return $ fromIntegral newIdx

getEntryById :: Connection -> Int -> IO (Maybe TodoEntry)
getEntryById dbConn rowId = do
  entries <- query dbConn "SELECT * FROM entries WHERE id = ?" (Only rowId)
  case entries of
    [x] -> return $ Just x
    _ -> return Nothing

updateEntry :: Connection -> TodoEntry -> IO ()
updateEntry dbConn entry =
  withTransaction dbConn $
    executeNamed
      dbConn
      "UPDATE entries SET checked = :checked WHERE id = :id"
      [":checked" := checked entry, ":id" := entryId entry]

deleteEntry :: Connection -> Int -> IO ()
deleteEntry dbConn rowId =
  execute dbConn "DELETE FROM entries WHERE id = ?" (Only rowId)
