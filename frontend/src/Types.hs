{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DeriveGeneric #-}

module Types where
import           Reflex.Dom hiding (Home)

import Model

import Control.Lens
import Data.Default
import GHC.Generics
import Data.Aeson

data BulkAction = AssignOwner | DeAssignOwner deriving (Show,Ord,Eq)

data NavPage = First
             | Previous
             | Next
             | Last
             deriving (Eq,Show,Enum)

data Nav = Home
         | Landing
         | Blog
         | Import
         | Kanban
         | Search
         | Acknowledgements
         | Register
         | Login
         deriving (Eq, Enum, Show) -- remember, Enum start from 0

data Env = Env  { _history :: [String]
                  , _auth :: (Maybe Token)
                  , _currentPage :: Int
                } deriving (Generic,Show)

$(makeLenses ''Env)

instance ToJSON Env
instance FromJSON Env

instance Reflex t => Default Env where
  def = Env { _history = []
            , _auth = Nothing
            , _currentPage = 1
            }

initEnv :: Env
initEnv = Env { _history = []
              , _auth = Nothing
              , _currentPage = 1
              }



