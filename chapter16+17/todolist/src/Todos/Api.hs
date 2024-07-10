{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Todos.Api
  ( TodoApi,
    todoApi,
  )
where

import Data.Proxy (Proxy (..))
import Servant.API
import Todos.Types (TodoEntry)

type TodoApi =
  "entries" :> Get '[JSON] [TodoEntry]
    :<|> "add" :> ReqBody '[JSON] TodoEntry :> PostCreated '[JSON] Int
    :<|> "toggle" :> Capture "id" Int :> PatchNoContent
    :<|> "delete" :> Capture "id" Int :> DeleteNoContent

todoApi :: Proxy TodoApi
todoApi = Proxy
