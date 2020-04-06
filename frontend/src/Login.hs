{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}

module Login where
import           Reflex.Dom hiding (Home)
import qualified Data.Map.Strict as Map
import           Data.Maybe (isJust)
--import           Data.FileEmbed

--import Data.Witherable

import Model
import Types
import Utils
import Settings

--import Control.Monad.Reader
import Control.Lens

loginPageWidget :: (MonadWidget t m) => Env t -> m (Event t ())
loginPageWidget dEnv = do
  el "div" $ text "LoginPage"
  display (dEnv ^. auth)
  username <- inputElement def
  password <- inputElement def

  eSend <- toButton "button" mempty $ text "Submit"
  let genLoginReq u p =
        mappend serverBackend $ "auth/signin?login=" <> u <> "&password=" <> p
  let dLoginReq = pure genLoginReq <*> value username <*> value password
  eToken :: Event t (Maybe Token) <- getAndDecode $ tag (current dLoginReq) eSend
  let (eNewXhr :: Event t (XhrRequestConfig ())) = attachWith putEnvToken (current $ dEnv ^. defXhrReqConfig) eToken
  dNewXhr <- holdDyn def eNewXhr
  dToken <- holdDyn Nothing eToken
  let dEnv2 = dEnv & auth .~ dToken
                   & defXhrReqConfig .~ dNewXhr
  let okToken = ffilter isJust $ updated dToken
  e <- toButton "button" mempty $ text "Back"
  let eRet = leftmost [e, () <$ okToken]
  return eRet
  where
    putEnvToken :: XhrRequestConfig () -> Maybe Token -> XhrRequestConfig ()
    putEnvToken d Nothing = d
    putEnvToken d (Just t) = d { _xhrRequestConfig_headers = Map.singleton "Authorization" $ token t }
