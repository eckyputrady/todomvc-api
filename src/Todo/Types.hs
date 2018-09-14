module Todo.Types where

import ClassyPrelude
import Control.Lens
import Data.Aeson
import Data.Generics.Product
import Data.Generics.Sum
import GHC.Generics (Generic)

data Todo = Todo
  { id :: Int
  , title :: Text
  , completed :: Bool
  } deriving (Generic, Show)

instance ToJSON Todo
instance FromJSON Todo

newtype CreateTodo = CreateTodo
  { title :: Text
  } deriving (Generic, Show)

instance ToJSON CreateTodo
instance FromJSON CreateTodo

data UpdateTodo = UpdateTodo
  { title :: Text
  , completed :: Bool
  } deriving (Generic, Show)

instance ToJSON UpdateTodo
instance FromJSON UpdateTodo
