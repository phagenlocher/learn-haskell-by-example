{-# LANGUAGE OverloadedStrings #-}

module Server (main) where

import Network.HTTP.Types.Header
import Network.HTTP.Types.Method
import Network.HTTP.Types.Status
import Network.Wai
import Network.Wai.Handler.Warp
import Options.Applicative
import Servant
import qualified Text.Blaze.Html.Renderer.Utf8 as H
import Todos.Api
import Todos.Database
import Todos.Handlers

mkApiServer :: Connection -> Application
mkApiServer = serve todoApi . mkTodoApiServer

mkAppRoot :: Connection -> Middleware
mkAppRoot dbConn app req responseAct
  | isRootRequest = do
      html <- rootHandler dbConn
      responseAct $ responseLBS ok200 headers (H.renderHtml html)
  | otherwise = app req responseAct
  where
    headers = [(hContentType, "text/html; charset=utf-8")]
    isRootRequest =
      requestMethod req == methodGet
        && null (pathInfo req)

data CliArgs = CliArgs
  { cliArgsPort :: Int,
    cliArgsDatabaseFile :: String
  }

cliArgsParser :: Parser CliArgs
cliArgsParser = CliArgs <$> port <*> databaseFile
  where
    port =
      option auto $
        long "port"
          <> value 9001
          <> metavar "PORT"
          <> help "Port to connect to"
          <> showDefault
    databaseFile =
      strOption $
        long "database"
          <> value ":memory:"
          <> metavar "PATH"
          <> help "Database file to use"
          <> showDefaultWith
            ( const
                "none, using in-memory database"
            )

argumentParser :: ParserInfo CliArgs
argumentParser =
  info
    (cliArgsParser <**> helper)
    ( fullDesc
        <> progDesc "A todo list API server"
    )

main :: IO ()
main = do
  args <- execParser argumentParser
  let port = cliArgsPort args
  withConnection (cliArgsDatabaseFile args) $ \dbConn -> do
    setupDb dbConn
    putStrLn $
      "Running todo list server on port " ++ show port
    run port (mkAppRoot dbConn $ mkApiServer dbConn)
