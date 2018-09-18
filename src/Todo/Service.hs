module Todo.Service where

import ClassyPrelude hiding (snoc)
import Todo.Types
import Control.Lens
import Data.Has
import Data.Generics.Product

type Deps r m = (Has (TVar State) r, MonadReader r m, MonadIO m)

data State = State
  { lastId :: Int
  , todos :: [Todo]
  } deriving (Generic, Show)

initialState :: State
initialState = State { lastId = 0, todos = [] }

withTVar :: Deps r m => (TVar State -> STM a) -> m a
withTVar f = do
  tvar <- asks getter
  atomically $ f tvar

addTodo :: Deps r m => CreateTodo -> m Todo
addTodo createTodo = withTVar $ \tvar -> do
  state <- readTVar tvar
  let newId = 1 + state ^. field @"lastId"
      newTodo = 
        Todo  { id = newId
              , title = createTodo ^. field @"title"
              , completed = False
              }
      newState = 
        State { lastId = newId
              , todos = snoc (state ^. field @"todos") newTodo
              }
  writeTVar tvar newState
  return newTodo

removeCompletedTodos :: Deps r m => m ()
removeCompletedTodos = withTVar $ \tvar -> 
  modifyTVar' tvar $ \state ->
    state & field @"todos" %~ (filter (not . getField @"completed"))

getAllTodos :: Deps r m => m [Todo]
getAllTodos = withTVar $ \tvar -> do
  state <- readTVar tvar
  return $ state ^. field @"todos"

getTodo :: Deps r m => Int -> m (Maybe Todo)
getTodo todoId = do
  todos <- getAllTodos
  return $ find (\todo -> todo ^. field @"id" == todoId) todos

updateTodo :: Deps r m => Todo -> m (Maybe Todo)
updateTodo newTodo = withTVar $ \tvar -> do
  state <- readTVar tvar
  let existingTodos = state ^. field @"todos"
      todoId = newTodo ^. field @"id"
      mayTodo = find (\todo -> todo ^. field @"id" == todoId) existingTodos
  case mayTodo of
    Nothing ->
      return Nothing
    Just _ -> do
      let replace todo =
            if todo ^. field @"id" == todoId
            then newTodo
            else todo
          newState = state & field @"todos" . traverse %~ replace
      writeTVar tvar newState
      return $ Just newTodo

removeTodo :: Deps r m => Int -> m ()
removeTodo todoId = withTVar $ \tvar ->
  modifyTVar' tvar $ \state ->
    state & field @"todos" %~ (filter (\todo -> todo ^. field @"id" /= todoId))

