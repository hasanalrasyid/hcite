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

import Types
import Utils

importPageWidget :: (MonadWidget t m) => Dynamic t Env -> m (Event t ())
importPageWidget dEnv = do -- Workflow . el "div" $ do
  el "div" $ text "ImportPage"
  display $ _auth <$> dEnv
  fi <- fileInput def
  eSubmit <- toButton "button" mempty $ text "Upload"
  let eFi = fmapMaybe listToMaybe $ tag (current $ value fi) eSubmit
  efd1 <- performEvent $ fmap (wrapFile "bib") eFi
  let efd = attachPromptlyDyn (_defXhrReqConfig <$> dEnv) efd1
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
