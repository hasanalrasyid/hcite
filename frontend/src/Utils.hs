{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Utils where

import Control.Applicative
import JSDOM.FormData as FD
import JSDOM.Types (File,MonadJSM)
import           Reflex.Dom hiding (Home)
import qualified Data.Text as T
import qualified Data.Map.Strict as Map
import           Data.Monoid ((<>))

import Servant.Links

import Data.Aeson.Types

import Model
import Types
import Routing
import Control.Lens
import Settings
import Reflex.Dom.Contrib.Widgets.CheckboxList (genCheckbox)

import Data.Maybe

hiddenDynAttrs :: Map.Map T.Text T.Text -> Bool -> Map.Map T.Text T.Text
hiddenDynAttrs initAttrs b =
  Map.adjust (<> isActive b)  "class" initAttrs
    where isActive True = " is-hidden"
          isActive _    = ""

--activateDynAttrs :: MonadWidget t m => Map.Map T.Text T.Text -> Bool -> Map.Map T.Text T.Text
activateDynAttrs :: Map.Map T.Text T.Text -> Bool -> Map.Map T.Text T.Text
activateDynAttrs initAttrs b =
  Map.adjust (<> isActive b)  "class" initAttrs
    where isActive True = " is-active"
          isActive _    = ""

toButton :: MonadWidget t m => T.Text -> Dynamic t (Map.Map T.Text T.Text) -> m a -> m (Event t ())
toButton d a t = do
  (e,_) <- elDynAttr' d a t
  return $ domEvent Click e





-- | Helper function to create a dynamic attribute map for the visibility of an element
visible :: Eq p => p -> p -> Map.Map T.Text T.Text
visible p1 p2 = "style" =: ("display: " <> choose (p1 == p2) "inline" "none")
  where
    choose True  t _ = t
    choose False _ f = f

noPageWidget :: (MonadWidget t m) => m (Event t ())
noPageWidget = do
  el "div" $ text "No Page So Far"
  toButton "button" (constDyn mempty) $ text "Home"

wrapFile :: (Monad m , MonadJSM m) => T.Text -> File -> m FD.FormData
wrapFile fname f = do
  fd <- FD.newFormData Nothing
  FD.appendBlob fd fname f (Nothing :: Maybe String)
  return fd

dViewArticle :: MonadWidget t m => Event t Bool -> Dynamic t SimpleRef
             -> m (Dynamic t (Int,Bool), Event t Int)
dViewArticle eCheck dRef = el "div" $ mdo
  -- dynText $ refTitle <$> dRef
  checkRef <- genCheckbox dynText (refTitle <$> dRef) eCheck -- $ _inputElement_checkedChange checkRef
  dynText $ refAuthor <$> dRef
  eAbstract <- toButton "div" mempty $ text "Abstract"
  dToggleAbstract <- toggle True eAbstract
  let eAbstractI = attachPromptlyDyn dToggleAbstract $ tag (current $ refSerial <$> dRef) eAbstract
  dAbstractI <- holdDyn (True,0) eAbstractI
  eAbstractT <- getAbstract3 dAbstractI
  elDynAttr "div" (hiddenDynAttrs ("class" =: "abstract") <$> dToggleAbstract) $ do
    elClass "hr" "login-hr" blank
    el "p" $ dynText =<< holdDyn "Init" eAbstractT

  eSerial <- toButton "div" mempty $
                elClass "i" "fa fa-edit" $ blank
  let serial = refSerial <$> dRef
  let dCheckRet = liftA2 (,) serial $ _inputElement_checked checkRef
  let eEdit = tag (current serial) eSerial
  return (dCheckRet, eEdit)

getAndDecodeSimpleRef :: (MonadWidget t m , FromJSON j, IsXhrPayload a) => Event t (XhrRequest a) -> m (Event t (Maybe [j]))
getAndDecodeSimpleRef d = do
  r <- performRequestAsync d
  return $ fmap decodeXhrResponse r

dViewOwnerPicker :: MonadWidget t m =>  Event t (SearchMode,T.Text) -> m (Dynamic t Int)
dViewOwnerPicker eOwnerSearch =
  el "div" $ mdo
    let eGetOwnerList1 = ffilter (\(m,x) -> (m == SOwner) && (T.length x > 2)) eOwnerSearch
    eGetOwnerList <- performRequestAsync $ ffor eGetOwnerList1 $ \s ->
        postJson (textFromJsonApi jsonApiGetPerson) $ Model.Search SAuthor $ snd s

    dGetOwnerList :: Dynamic t [Person] <- holdDyn [] $ fforMaybe eGetOwnerList decodeXhrResponse
    dleSetOwner <- flip simpleList dView dGetOwnerList
    let deSetOwner = fmap leftmost dleSetOwner
        eSetOwner  = switchDyn deSetOwner
    dSetOwner <- holdDyn 0 eSetOwner
    return dSetOwner
    where
      dView o = do
        e <- toButton "div" mempty $ dynText $ fmap personName o
        return $ tag (current $ fmap personId o) e

bulkExecute :: MonadWidget t m => Dynamic t (Maybe Env) -> Dynamic t Int -> Dynamic t [(Int,Bool)] -> Event t BulkAction -> m ()
bulkExecute dEnv dOwner dFilteredBulk eBulkExecute = mdo
  let efd = attach (current dEnv) $ attach (current dOwner) $ attach (current dFilteredBulk) eBulkExecute
  --dAuth :: Dynamic t (Maybe Token) <- askStorageTagDef Tag1 Nothing
  r <- performRequestAsync $ ffor efd
        $ \(mEnv,(owner,(lRefCheck,actBulk))) ->
          let (jsonApi,mApi) = case actBulk of
                          AssignOwner -> (jsonApiPutOwner,"PUT")
                          DeAssignOwner -> (jsonApiDeleteOwner,"DELETE")
              postJsonReq = postJson (textFromJsonApi jsonApi) $ OwnerLRef owner $ map fst lRefCheck
              postReq = postJsonReq & xhrRequest_method .~ mApi
              mAuth = case mEnv of
                        Nothing -> Token ""
                        Just e  -> fromMaybe (Token "") $ _auth e
          in postReq & xhrRequest_config . xhrRequestConfig_headers <>~ (Map.singleton  "Authorization"  $ token mAuth)
                --defXhr ^. xhrRequestConfig_headers

  st :: Dynamic t [(T.Text,T.Text)] <- holdDyn [] $ fforMaybe r decodeXhrResponse
  display st

textFromJsonApi :: Servant.Links.Link -> T.Text
textFromJsonApi j = mappend serverBackend $ T.pack $ show $ linkURI $ j

getAbstract3 :: MonadWidget t m => Dynamic t (Bool,Int) -> m (Event t T.Text)
getAbstract3 d = do
  let e1 = ffilter (not . fst) $ updated d
  e2 <- getAndDecode (absAddress <$> e1)
  return $ cekR3 <$> e2
    where
      absAddress (_,i) = (mappend serverBackend $ T.pack $ show $ linkURI $ jsonApiGetAbstract i)
      cekR3 Nothing = "Unavailable"
      cekR3 (Just a) = absAbstract a
