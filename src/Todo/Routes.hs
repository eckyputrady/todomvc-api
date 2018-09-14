module Todo.Routes where

import ClassyPrelude hiding (delete)
import Control.Lens
import Data.Generics.Product hiding (param)
import Web.Scotty.Trans
import Network.HTTP.Types.Status
import Network.Wai
import Todo.Types

class Monad m => Service m where
  addTodo :: CreateTodo -> m Todo
  removeCompletedTodos :: m ()
  getAllTodos :: m [Todo]
  getTodo :: Int -> m (Maybe Todo)
  updateTodo :: Todo -> m (Maybe Todo)
  removeTodo :: Int -> m ()

routes :: (Service m, MonadIO m) => ScottyT LText m ()
routes = do
    
  post "/todos" $ do
    arg <- jsonData
    result <- lift $ addTodo arg
    status status201
    json result

  get "/todos" $ do
    result <- lift $ getAllTodos
    json result

  delete "/todos" $ do
    lift $ removeCompletedTodos
    status status204

  get "/todos/:id" $ do
    todoId <- param "id"
    mayResult <- lift $ getTodo todoId
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
    mayResult <- lift $ updateTodo todo
    case mayResult of
      Nothing ->
        status status404
      Just result ->
        json result

  delete "/todos/:id" $ do
    todoId <- param "id"
    lift $ removeTodo todoId
    status status204

