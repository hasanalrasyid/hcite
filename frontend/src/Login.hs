{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}

module Login where
import           Reflex.Dom hiding (Home)

import Model
import Utils
import Settings

import Storage.Example
import Reflex.Dom.Storage.Class
import Data.Maybe
import Types

loginPageWidget :: (HasStorage t ExampleTag m, MonadWidget t m) => m (Event t ())
loginPageWidget = do
  el "div" $ text "LoginPage"
  username <- inputElement def
  password <- inputElement def

  eSend <- toButton "button" mempty $ text "Submit"
  let genLoginReq u p =
        mappend serverBackend $ "auth/signin?login=" <> u <> "&password=" <> p
  let dLoginReq = pure genLoginReq <*> value username <*> value password
  eToken :: Event t (Maybe Token) <- getAndDecode $ tag (current dLoginReq) eSend
    {-
  let (eNewXhr :: Event t (XhrRequestConfig ())) = attachWith putEnvToken (current $ _defXhrReqConfig <$> dEnv) eToken
  dNewXhr <- holdDyn def eNewXhr
  -}
  oldEnv <- askStorageTagDef Tag1 Nothing
  let newEnv = genNewEnv <$> attach (current oldEnv) eToken
  tellStorageInsert Tag1 newEnv

  {-
  let (eEnv2 :: Event t Env) = attachWith setNewEnv (current dToken) $ attach (current dNewXhr) $ updated dEnv
  dEnv2 <- holdDyn initEnv eEnv2
  display dEnv2
  -}
  let eEnv2 = ffilter isJust eToken
  e <- toButton "button" mempty $ text "Back"
  let eRet = leftmost [() <$ eEnv2, e]
  return $ eRet
  where
    genNewEnv (e,t) =
      let env = case e of
                  Nothing -> initEnv
                  Just en -> en
       in Just $ env & auth .~ t
    {-
    setNewEnv t (x,e) = e { _auth = t
                          , _defXhrReqConfig = x
                          }
    putEnvToken :: XhrRequestConfig () -> Maybe Token -> XhrRequestConfig ()
    putEnvToken d Nothing = d
    putEnvToken d (Just t) = d { _xhrRequestConfig_headers = Map.singleton "Authorization" $ token t }
                          -}

