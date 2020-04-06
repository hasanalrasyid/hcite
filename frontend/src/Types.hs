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

data Env t = Env  { _history :: [String]
                  , _auth :: Dynamic t (Maybe Token)
                  , _defXhrReqConfig :: Dynamic t (XhrRequestConfig ())
               }

$(makeLenses ''Env)

instance Reflex t => Default (Env t) where
  def = Env { _history = []
            , _auth = constDyn Nothing
            , _defXhrReqConfig = constDyn def
            }

