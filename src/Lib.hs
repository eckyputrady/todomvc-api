module Lib where

import ClassyPrelude
import Todo.Routes as TodoR
import Todo.Service as TodoS
import Web.Scotty.Trans

type Env = TVar TodoS.State

newtype App a = App
  { unApp :: ReaderT Env IO a
  } deriving
  ( Applicative, Functor, Monad, MonadReader Env, MonadIO
  )

runApp :: Env -> App a -> IO a
runApp env app =
  runReaderT (unApp app) env

main :: IO ()
main = do
  env <- newTVarIO TodoS.initialState
  scottyT 3000 (runApp env) TodoR.routes

----

instance TodoR.Service App where
  addTodo = TodoS.addTodo
  removeCompletedTodos = TodoS.removeCompletedTodos
  getAllTodos = TodoS.getAllTodos
  getTodo = TodoS.getTodo
  updateTodo = TodoS.updateTodo
  removeTodo = TodoS.removeTodo
