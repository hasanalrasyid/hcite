{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}

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
import Utils

--import Control.Monad.Reader
import Control.Lens
import Control.Applicative
import Data.Default
import Reflex.Dom.Contrib.Widgets.EditInPlace (editInPlace)
import JSDOM.FormData as FD
import JSDOM.Types (File,MonadJSM)
import Reflex.Dom.Contrib.Widgets.CheckboxList (genCheckbox)

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

main :: IO ()
main = mainWidgetWithHead headElement body

headElement :: MonadWidget t m => m ()
headElement = do
  {-
<meta charset="utf-8">
<meta http-equiv="X-UA-Compatible" content="IE=edge">
<meta name="viewport" content="width=device-width, initial-scale=1">
-}
  el "title" $ text "Main Title"
  stylesheet "inc/css/font-awesome.min.css"
  stylesheet "inc/css/OpenSans.css"
  stylesheet "inc/css/bulma.min.css"
  stylesheet "inc/css/bulma-checkradio.min.css"
  stylesheet "inc/css/custom.css"
--  stylesheet "inc/css/forum.css"
--  stylesheet "inc/css/bulma-docs.min.css"
  where
    stylesheet l = elAttr "link" (Map.fromList [
          ("rel", "stylesheet")
        , ("type", "text/css")
        , ("href", l)
      ]) $ return ()

body :: MonadWidget t m => m ()
body = mdo
  eStart <- getPostBuild
  -- Builds up a `Dynamic` of widgets that return `Event t Text`:
  dWidget <- holdDyn homeWidget . leftmost $ [
      homeWidget <$ eStart
    --, unimplementedWidget <$ eUnimplemented
    ]

  -- Using `dyn` on this gives us an `Event t (Event t Text)`:
  eeDetail <- dyn dWidget
  -- and we can use `switchHold` to turn that into an `Event t Int`:
  eDetail <- switchHold never eeDetail

  -- dText hanya untuk penguat saja. sapa tahu event perubahan ini diperlukan
  dText <- holdDyn "" . leftmost $ [
               "eText" <$  eDetail
             , "eSwitch" <$ eStart
             ]

  el "div" $
    dynText dText

  {-

  r <- workflow page1
  el "div" $ do
    text "Current page is: "
    dynText r
-}

homeWidget :: MonadWidget t m => m (Event t T.Text)
homeWidget = do
  r <- workflow $ homePage $ def
  display r
  return $ updated r

data BulkAction = AssignOwner | DeAssignOwner deriving (Show,Ord,Eq)

noPage :: (MonadWidget t m) => (Env t) -> Workflow t m T.Text
noPage dEnv = Workflow . el "div" $ do
  el "div" $ text "No Page So Far"
  display (dEnv ^. auth)
  e <- toButton "button" (constDyn mempty) $ text "Home"
  return ("noPage", homePage dEnv <$ e)

wrapFile :: (Monad m , MonadJSM m) => T.Text -> File -> m FD.FormData
wrapFile fname f = do
  fd <- FD.newFormData Nothing
  FD.appendBlob fd fname f (Nothing :: Maybe String)
  return fd

importPage :: (MonadWidget t m) => (Env t) -> Workflow t m T.Text
importPage dEnv = Workflow . el "div" $ do
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
  return ("ImportPage", homePage dEnv <$ e)

loginPage :: (MonadWidget t m) => (Env t) -> Workflow t m T.Text
loginPage dEnv = Workflow . el "div" $ do
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
  return ("LoginPage", homePage dEnv2 <$ eRet)
  where
    putEnvToken :: XhrRequestConfig () -> Maybe Token -> XhrRequestConfig ()
    putEnvToken d Nothing = d
    putEnvToken d (Just t) = d { _xhrRequestConfig_headers = Map.singleton "Authorization" $ token t }

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

serverBackend :: T.Text
--serverBackend = "http://127.0.0.1:3000/"
serverBackend = "http://192.168.43.175:3000/"

getAndDecodeSimpleRef :: (MonadWidget t m , IsXhrPayload a) => Event t (XhrRequest a) -> m (Event t (Maybe [SimpleRef]))
getAndDecodeSimpleRef d = do
  r <- performRequestAsync d
  return $ fmap decodeXhrResponse r


homePage :: MonadWidget t m => (Env t) ->  Workflow t m T.Text
homePage dEnv = Workflow $ do
  eNav <- bodyNav
  let eHome = ffilter (== Home) eNav
  let eLogin = ffilter (== Login) eNav
  let eImport = ffilter (== Import) eNav
  el "div" $ mdo
    text "home"
    display (dEnv ^. auth)
    eStart <- getPostBuild

    drModel <- dropdown SAbstract
                        (constDyn $ Map.fromList [(SAbstract,"abstract")
                                                 ,(SAuthor,"author")
                                                 ,(SKeywords,"keywords")
                                                 ,(SOwner,"owner")])
                        def

    tOwnerSearch <- inputElement def
    eSearchButton <- toButton "button" mempty $ text "Search"
    let eSearch = leftmost [eSearchButton, eStart]


    let ePostXhrRequest = fmap genSearchReq
                              $ attach (current dTOwner)
                              $ attach (current $ _dropdown_value drModel)
                              $ tag (current $ _inputElement_value tOwnerSearch) eSearch

    eRefList <- getAndDecodeSimpleRef ePostXhrRequest
    dR <- holdDyn Nothing eRefList
    let dRefListSearch =fromMaybe [] <$> dR


    dropdownBulkAction <- dropdown AssignOwner
                        (constDyn $ Map.fromList [(AssignOwner,"Assign Owner")
                                                 ,(DeAssignOwner,"Remove Ownership")
                                                 ])
                        def
    dTOwner <- dViewOwnerPicker (attach (current $ _dropdown_value drModel) $ _inputElement_input tOwnerSearch)
    display dTOwner
    display $ _dropdown_value dropdownBulkAction
    eBulkExecute <- toButton "button" mempty $ text "Execute"
    bulkExecute dEnv dTOwner (fmap (filter snd) dBulk) $ tag (current $ _dropdown_value dropdownBulkAction) eBulkExecute
    --assignOwner dEnv dTOwner (fmap (filter snd) dBulk) eAssignOwner

    bulkAll <- genCheckbox text "CheckAll" $ _inputElement_checkedChange bulkAll
    text "bulkAll"
    display $ _inputElement_checked bulkAll
    display dBulk
    dleEdit <- flip simpleList (dViewArticle (_inputElement_checkedChange bulkAll)) dRefListSearch
    let dBulk = fmap (map snd . Map.toList) $ joinDynThroughMap $ fmap (\x -> Map.fromList $ zip ([1..] :: [Int]) $ map fst x) dleEdit
    let deEdit = fmap (leftmost . (map snd)) dleEdit
        eEdit = switchDyn deEdit
    dEdit <- holdDyn 0 eEdit

    let thePage = leftmost $ [detailPage dEnv dEdit <$ eEdit,homePage dEnv <$ eHome, loginPage dEnv <$ eLogin, importPage dEnv <$ eImport, noPage dEnv <$ eNav]
    return ("HomePage", thePage)
    where
      genSearchReq (o,(m,s)) =
        let target = case m of
                       SOwner -> textFromJsonApi $ jsonApiGetListOwnerId 1 o
                       _ -> textFromJsonApi $ jsonApiGetListSearch 1
         in postJson target $ Model.Search m s






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

bulkExecute :: MonadWidget t m => Env t -> Dynamic t Int -> Dynamic t [(Int,Bool)] -> Event t BulkAction -> m ()
bulkExecute dEnv dOwner dFilteredBulk eBulkExecute = mdo
  let efd = attach (current $ dEnv ^. defXhrReqConfig) $ attach (current dOwner) $ attach (current dFilteredBulk) eBulkExecute
  r <- performRequestAsync $ ffor efd
        $ \(defXhr,(owner,(lRefCheck,actBulk))) ->
          let (jsonApi,mApi) = case actBulk of
                          AssignOwner -> (jsonApiPutOwner,"PUT")
                          DeAssignOwner -> (jsonApiDeleteOwner,"DELETE")
              postJsonReq = postJson (textFromJsonApi jsonApi) $ OwnerLRef owner $ map fst lRefCheck
              postReq = postJsonReq & xhrRequest_method .~ mApi
          in postReq & xhrRequest_config . xhrRequestConfig_headers <>~ defXhr ^. xhrRequestConfig_headers

  st :: Dynamic t [(T.Text,T.Text)] <- holdDyn [] $ fforMaybe r decodeXhrResponse
  display st

textFromJsonApi :: Servant.Links.Link -> T.Text
textFromJsonApi j = mappend serverBackend $ T.pack $ show $ linkURI $ j

detailPage :: (MonadWidget t m) => (Env t) -> Dynamic t Int -> Workflow t m T.Text
detailPage dEnv dSerial = Workflow . el "div" $ do
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
  return ("DetailPage", homePage dEnv <$ eBack)

getAbstract3 :: MonadWidget t m => Dynamic t (Bool,Int) -> m (Event t T.Text)
getAbstract3 d = do
  let e1 = ffilter (not . fst) $ updated d
  e2 <- getAndDecode (absAddress <$> e1)
  return $ cekR3 <$> e2
    where
      absAddress (_,i) = (mappend serverBackend $ T.pack $ show $ linkURI $ jsonApiGetAbstract i)
      cekR3 Nothing = "Unavailable"
      cekR3 (Just a) = absAbstract a

