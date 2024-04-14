module Client (main) where

import qualified Data.Text as T
import Network.HTTP.Client
  ( defaultManagerSettings,
    newManager,
  )
import Options.Applicative
import Servant
import Servant.Client
import Todos.Api
import Todos.Types

getEntries :: ClientM [TodoEntry]
addEntry :: TodoEntry -> ClientM Int
toggleEntry :: Int -> ClientM NoContent
deleteEntry :: Int -> ClientM NoContent
getEntries :<|> addEntry :<|> toggleEntry :<|> deleteEntry = client todoApi

data CliArgs = CliArgs
  { cliArgsHost :: String,
    cliArgsPort :: Int,
    cliArgsCommand :: CliCommand
  }

data CliCommand
  = GetEntries
  | AddEntry TodoEntry
  | ToggleEntry Int
  | DeleteEntry Int

cliArgsParser :: Parser CliArgs
cliArgsParser =
  CliArgs
    <$> host
    <*> port
    <*> commandParser
  where
    host =
      strOption $
        long "host"
          <> value "localhost"
          <> metavar "HOST"
          <> help "host to connect to"
          <> showDefault
    port =
      option auto $
        long "port"
          <> value 9001
          <> metavar "PORT"
          <> help "port to connect to"
          <> showDefault
    commandParser =
      subparser
        ( command "list" getEntriesCommandParser
            <> command "add" addEntryCommandParser
            <> command "toggle" toggleEntryCommandParser
            <> command "delete" deleteEntryCommandParser
        )

getEntriesCommandParser :: ParserInfo CliCommand
getEntriesCommandParser =
  info
    (pure GetEntries <**> helper)
    ( fullDesc
        <> progDesc "Retrieves and lists all todo list entries"
    )

addEntryCommandParser :: ParserInfo CliCommand
addEntryCommandParser =
  info
    ((AddEntry <$> todoEntryParser) <**> helper)
    ( fullDesc
        <> progDesc "Adds a new entry to the todo list"
    )

toggleEntryCommandParser :: ParserInfo CliCommand
toggleEntryCommandParser =
  info
    ((ToggleEntry <$> idParser) <**> helper)
    ( fullDesc
        <> progDesc "(Un)checks the entry with the given id"
    )
  where
    idParser =
      option auto $
        long "id"
          <> metavar "INT"
          <> help "Row id to toggle"

deleteEntryCommandParser :: ParserInfo CliCommand
deleteEntryCommandParser =
  info
    ((DeleteEntry <$> idParser) <**> helper)
    ( fullDesc
        <> progDesc "Deletes the entry with the given id"
    )
  where
    idParser =
      option auto $
        long "id"
          <> metavar "INT"
          <> help "Row id to delete"

todoEntryParser :: Parser TodoEntry
todoEntryParser =
  TodoEntry
    <$> teEntryId
    <*> (T.pack <$> teText)
    <*> teChecked
    <*> teRank
  where
    teEntryId =
      optional $
        option auto $
          long "id"
            <> metavar "INT"
            <> help "Row id to use (optional)"
    teText =
      strOption $
        long "text"
          <> metavar "TEXT"
          <> help "What is there to do?"
    teChecked =
      option auto $
        long "checked"
          <> metavar "BOOL"
          <> help "Is it done?"
    teRank =
      optional $
        option auto $
          long "rank"
            <> metavar "INT"
            <> help "Sorting rank (optional)"

argumentParser :: ParserInfo CliArgs
argumentParser =
  info
    (cliArgsParser <**> helper)
    ( fullDesc
        <> progDesc "A todo list API client"
    )

main :: IO ()
main = do
  args <- execParser argumentParser
  clientManager <- newManager defaultManagerSettings
  let url = BaseUrl Http (cliArgsHost args) (cliArgsPort args) ""
      clientEnv = mkClientEnv clientManager url
      runClientM' :: ClientM a -> (a -> IO ()) -> IO ()
      runClientM' clientAct resAct = do
        res <- runClientM clientAct clientEnv
        case res of
          Left err -> putStrLn $ "Error: " ++ show err
          Right x -> resAct x
  case cliArgsCommand args of
    GetEntries ->
      runClientM' getEntries $ mapM_ print . sortTodoEntries
    AddEntry entry ->
      runClientM' (addEntry entry) print
    ToggleEntry rowId ->
      runClientM' (toggleEntry rowId) (const $ pure ())
    DeleteEntry rowId ->
      runClientM' (deleteEntry rowId) (const $ pure ())
