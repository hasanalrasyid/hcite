{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}

module Types where
import           Reflex.Dom hiding (Home)

import Model

import Control.Lens
import Data.Default

data BulkAction = AssignOwner | DeAssignOwner deriving (Show,Ord,Eq)

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
                  , _defXhrReqConfig :: (XhrRequestConfig ())
                }

$(makeLenses ''Env)

instance Reflex t => Default Env where
  def = Env { _history = []
            , _auth = Nothing
            , _defXhrReqConfig = def
            }

initEnv :: Env
initEnv = Env { _history = []
              , _auth = Nothing
              , _defXhrReqConfig = def
              }



