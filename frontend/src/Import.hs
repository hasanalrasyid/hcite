{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}

module Import where
import           Reflex.Dom hiding (Home)
import qualified Data.Text as T
import           Data.Maybe (listToMaybe)

import Routing

import Utils

importPageWidget :: (MonadWidget t m) =>  m (Event t ())
importPageWidget = do -- Workflow . el "div" $ do
  el "div" $ text "ImportPage"
  fi <- fileInput def
  eSubmit <- toButton "button" mempty $ text "Upload"
  let eFi = fmapMaybe listToMaybe $ tag (current $ value fi) eSubmit
  efd <- performEvent $ fmap (wrapFile "bib") eFi
  r <- performRequestAsync $ ffor efd $ \fd ->
        let postReq = textFromJsonApi jsonApiPutFile
         in xhrRequest "POST" postReq $ def {
                          _xhrRequestConfig_sendData = fd
                         }
  st :: Dynamic t [(T.Text,T.Text)] <- holdDyn [] $ fforMaybe r decodeXhrResponse
  el "p" $ do
    text "Upload status:"
    dynText $ fmap (T.pack . show) st

  e <- toButton "button" (constDyn mempty) $ text "Back"
  return e
--  return ("ImportPage", backFunc <$ e)
