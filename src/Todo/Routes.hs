module Todo.Routes where

import ClassyPrelude hiding (delete)
import Control.Lens
import Data.Generics.Product hiding (param)
import Web.Scotty.Trans
import Network.HTTP.Types.Status
import Network.Wai
import Todo.Types
import qualified Todo.Service as S

routes :: S.Deps r m => ScottyT LText m ()
routes = do
    
  post "/todos" $ do
    arg <- jsonData
    result <- lift $ S.addTodo arg
    status status201
    json result

  get "/todos" $ do
    result <- lift $ S.getAllTodos
    json result

  delete "/todos" $ do
    lift $ S.removeCompletedTodos
    status status204

  get "/todos/:id" $ do
    todoId <- param "id"
    mayResult <- lift $ S.getTodo todoId
    case mayResult of
      Nothing ->
        status status404
      Just result ->
        json result

  put "/todos/:id" $ do
    todoId <- param "id"
    arg :: UpdateTodo <- jsonData
    let todo = Todo { id = todoId
                    , title = arg ^. field @"title"
                    , completed = arg ^. field @"completed"
                    }
    mayResult <- lift $ S.updateTodo todo
    case mayResult of
      Nothing ->
        status status404
      Just result ->
        json result

  delete "/todos/:id" $ do
    todoId <- param "id"
    lift $ S.removeTodo todoId
    status status204

