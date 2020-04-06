{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}

module Home.Detail where
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

detailPageWidget :: (MonadWidget t m) => (Env t) -> Dynamic t Int -> m (Event t ())
detailPageWidget dEnv dSerial = do
  display (dEnv ^. auth)
  eBack <- toButton "div" mempty $ text "Back"
  el "div" $ text "You have arrived on page 3"
  let tGetSingle = textFromJsonApi . jsonApiGetSingle

  eStart <- getPostBuild
  display $ tGetSingle <$> dSerial
  eRef1 :: Event t (Maybe Reference) <- getAndDecode $
    tGetSingle <$> tag (current dSerial) eStart
  let eRef = mapMaybe id eRef1

  dRefs <- holdDyn [] $ (:[]) <$> eRef
  dTitle <- holdDyn "TITLE_BLANK" $ referenceTitle <$> mapMaybe id eRef1
  eTitle <- editInPlace (constant True) dTitle -- $ (referenceTitle <$> dRef)

  el "hr" blank
  display =<< holdDyn "eTitleHold" eTitle
  el "hr" blank
  display dRefs
  return eBack
