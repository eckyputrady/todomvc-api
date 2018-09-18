module Lib (main) where

import ClassyPrelude
import Todo.Routes as TodoR
import Todo.Service as TodoS
import Web.Scotty.Trans

type Env = TVar TodoS.State
type App a = ReaderT Env IO a

runApp :: Env -> App a -> IO a
runApp = flip runReaderT

main :: IO ()
main = do
  env <- newTVarIO TodoS.initialState
  scottyT 3000 (runApp env) TodoR.routes