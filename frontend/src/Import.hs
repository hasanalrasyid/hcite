{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}

module Import where
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

import Navigation
import Types
import Utils
import Settings

--import Control.Monad.Reader
import Control.Lens
import Control.Applicative
import Data.Default
import Reflex.Dom.Contrib.Widgets.EditInPlace (editInPlace)
import JSDOM.FormData as FD
import JSDOM.Types (File,MonadJSM)
import Reflex.Dom.Contrib.Widgets.CheckboxList (genCheckbox)

importPageWidget :: (MonadWidget t m) => (Env t) -> m (Event t ())
importPageWidget dEnv = do -- Workflow . el "div" $ do
  el "div" $ text "ImportPage"
  display (dEnv ^. auth)
  fi <- fileInput def
  eSubmit <- toButton "button" mempty $ text "Upload"
  let eFi = fmapMaybe listToMaybe $ tag (current $ value fi) eSubmit
  efd1 <- performEvent $ fmap (wrapFile "bib") eFi
  let efd = attachPromptlyDyn (dEnv ^. defXhrReqConfig) efd1
  r <- performRequestAsync $ ffor efd $ \(defXhr,fd) ->
        xhrRequest "POST" (textFromJsonApi jsonApiPutFile) $ defXhr {
                          _xhrRequestConfig_sendData = fd
                         }
  st :: Dynamic t [(T.Text,T.Text)] <- holdDyn [] $ fforMaybe r decodeXhrResponse
  el "p" $ do
    text "Upload status:"
    dynText $ fmap (T.pack . show) st

  e <- toButton "button" (constDyn mempty) $ text "Back"
  return e
--  return ("ImportPage", backFunc <$ e)
