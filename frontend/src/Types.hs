{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}

module Types where
import           Reflex.Dom hiding (Home)
import qualified Data.Text as T
import qualified Data.Map.Strict as Map
import           Data.Maybe (fromMaybe,listToMaybe,isJust)
--import           Data.FileEmbed

--import Data.Witherable

import Routing
import Model
--import Reflex.Dom.Xhr

--import Language.Javascript.JSaddle.Types
--import Control.Monad.IO.Class
--import Control.Monad
--import Control.Monad.Trans

import Servant.Links

--import Reflex.Bulmex.Modal
--import Reflex.Bulmex.Tag.Bulma

--import Proto (toButton,hiddenDynAttrs,bodyNav,Nav(..))

--import Control.Monad.Reader
import Control.Lens
import Control.Applicative
import Data.Default
import Reflex.Dom.Contrib.Widgets.EditInPlace (editInPlace)
import JSDOM.FormData as FD
import JSDOM.Types (File,MonadJSM)
import Reflex.Dom.Contrib.Widgets.CheckboxList (genCheckbox)

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

