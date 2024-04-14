{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Todos.Handlers
  ( rootHandler,
    entriesHandler,
    addHandler,
    toggleHandler,
    mkTodoApiServer,
  )
where

import Control.Monad.IO.Class
import qualified Data.ByteString.Lazy as BSL
import Data.String (fromString)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Servant
import qualified Text.Blaze.Html5 as H
import Todos.Api
import Todos.Database
import Todos.Types

rootHandler :: Connection -> IO H.Html
rootHandler dbConn = do
  entries <- sortTodoEntries <$> getEntries dbConn
  let listHtml
        | null entries = H.p $ H.text "No entry found"
        | otherwise = H.ul $ mapM_ (H.li . mkTodoEntryHtml) entries
      fullHtml = H.docTypeHtml $ do
        H.head $ do
          H.title "TODOs"
          H.style $ H.text style
        H.body $ do
          H.h1 "TODOs"
          listHtml
  return fullHtml
  where
    style :: T.Text
    style =
      "body {font-family: sans-serif} \
      \ ul {list-style: none} \
      \ li {margin-bottom: 5px}"

    mkTodoEntryHtml :: TodoEntry -> H.Html
    mkTodoEntryHtml (TodoEntry _ text checked _) =
      let mark =
            if checked
              then "\x2713" -- Check mark (U+2713)
              else "\x2718" -- Cross mark (U+2718)
       in H.text (mark <> " " <> text)

entriesHandler :: IO [TodoEntry] -> Handler [TodoEntry]
entriesHandler = liftIO

addHandler ::
  (TodoEntry -> IO (Either SQLError Int)) ->
  TodoEntry ->
  Handler Int
addHandler addAct entry = do
  mSqlError <- liftIO $ addAct entry
  case mSqlError of
    Right idx -> pure idx
    Left exc ->
      let errBody =
            BSL.fromStrict
              (T.encodeUtf8 $ sqlErrorDetails exc)
       in throwError $ err422 {errBody}

toggleHandler ::
  (Int -> IO (Maybe TodoEntry)) ->
  (TodoEntry -> IO ()) ->
  Int ->
  Handler NoContent
toggleHandler queryAct updateAct rowId = do
  mEntry <- liftIO $ queryAct rowId
  case mEntry of
    Nothing ->
      let errBody =
            fromString $
              "Couldn't find entry with id \""
                <> show rowId
                <> "\""
       in throwError $ err404 {errBody}
    Just entry -> do
      let newEntry = entry {checked = not $ checked entry}
      liftIO $ updateAct newEntry
      pure NoContent

deleteHandler ::
  (Int -> IO ()) ->
  Int ->
  Handler NoContent
deleteHandler deleteAct rowId = do
  liftIO $ deleteAct rowId
  pure NoContent

mkTodoApiServer :: Connection -> Server TodoApi
mkTodoApiServer dbConn =
  entriesHandler getEntries'
    :<|> addHandler addEntry'
    :<|> toggleHandler getEntryById' updateEntry'
    :<|> deleteHandler deleteEntry'
  where
    getEntries' = getEntries dbConn
    addEntry' = addEntry dbConn
    getEntryById' = getEntryById dbConn
    updateEntry' = updateEntry dbConn
    deleteEntry' = deleteEntry dbConn
