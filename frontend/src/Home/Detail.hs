{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}

module Home.Detail where
import           Reflex.Dom hiding (Home)

import Routing
import Model

import Types
import Utils

import Control.Lens
import Reflex.Dom.Contrib.Widgets.EditInPlace (editInPlace)

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
