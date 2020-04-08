{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}

module Login where
import           Reflex.Dom hiding (Home)
import qualified Data.Map.Strict as Map

import Model
import Types
import Utils
import Settings

import Storage.Example
import Reflex.Dom.Storage.Class

loginPageWidget :: (HasStorage t ExampleTag m, MonadWidget t m) => Dynamic t Env -> m (Event t Env)
loginPageWidget dEnv = do
  el "div" $ text "LoginPage"
  display $ _auth <$> dEnv
  username <- inputElement def
  password <- inputElement def

  eSend <- toButton "button" mempty $ text "Submit"
  let genLoginReq u p =
        mappend serverBackend $ "auth/signin?login=" <> u <> "&password=" <> p
  let dLoginReq = pure genLoginReq <*> value username <*> value password
  eToken :: Event t (Maybe Token) <- getAndDecode $ tag (current dLoginReq) eSend
  let (eNewXhr :: Event t (XhrRequestConfig ())) = attachWith putEnvToken (current $ _defXhrReqConfig <$> dEnv) eToken
  dNewXhr <- holdDyn def eNewXhr
  dToken <- holdDyn Nothing eToken

  tellStorageInsert Tag1 eToken

  let (eEnv2 :: Event t Env) = attachWith setNewEnv (current dToken) $ attach (current dNewXhr) $ updated dEnv
  e <- toButton "button" mempty $ text "Back"
  dEnv2 <- holdDyn initEnv eEnv2
  display dEnv2
  let eRet = leftmost [eEnv2, initEnv <$ e]
  return $ eRet
  where
    setNewEnv t (x,e) = e { _auth = t
                          , _defXhrReqConfig = x
                          }
    putEnvToken :: XhrRequestConfig () -> Maybe Token -> XhrRequestConfig ()
    putEnvToken d Nothing = d
    putEnvToken d (Just t) = d { _xhrRequestConfig_headers = Map.singleton "Authorization" $ token t }
