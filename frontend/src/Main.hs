{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}

import           Reflex.Dom hiding (Home)
import qualified Data.Text as T
import qualified Data.Map.Strict as Map
import           Data.Maybe (fromMaybe,listToMaybe)

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

import Proto hiding (main,headElement,body)
import Home  hiding (main,headElement,body,homePage,homeWidget,detailPage)

--import Control.Monad.Reader
import Control.Lens
import Data.Default
import Reflex.Dom.Contrib.Widgets.EditInPlace (editInPlace)
import JSDOM.FormData as FD
import JSDOM.Types (File,MonadJSM)


data Env t = Env  { _history :: [String]
                  , _auth :: Dynamic t (Maybe Token)
                  , _defXhrRequest :: Dynamic t (XhrRequestConfig ())
               }

$(makeLenses ''Env)

instance Reflex t => Default (Env t) where
  def = Env { _history = []
            , _auth = constDyn Nothing
            , _defXhrRequest = constDyn def
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
    {-
  eNav :: Event t Nav <- bodyNav
  let eHome = ffilter (== Home) eNav
  let eLogin = ffilter (== Login) eNav
      eUnimplemented = ffilter (flip notElem navMenuImplemented) eNav
      eCurrent = leftmost [Right <$> eHome, Left <$> eUnimplemented]

  dUnimplemented <- holdDyn Nothing $ fmap Just eUnimplemented
  dCurrent <- holdDyn (Left Home) $ eCurrent

  dNav <- holdDyn Home eNav
  display dNav
  -}
  -- Builds up a `Dynamic` of widgets that return `Event t Text`:
  dWidget <- holdDyn homeWidget . leftmost $ [
      homeWidget <$ eStart
    --, unimplementedWidget <$ eUnimplemented
    ]

-- di sini, textWidget dan textWidget2 return Event t Int from referenceSerial.
-- Sebab isi textWidget* adalah:
-- v. HomePage
-- v. LoginPage
-- v. DetailPage (bisa static)
-- v. DetailPage (bisa edited)
-- 4. DetailedSearchPage
-- 5. SearchResultPage (seperti HomePage tapi content yang berbeda)
-- v. ImportPage (ini yang paling penting sebenarnya)
-- 7. Full Editor
-- 8. Cosmetics
--
--

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
  eSubmit <- button "Upload"
  let eFi = fmapMaybe listToMaybe $ tag (current $ value fi) eSubmit
  efd1 <- performEvent $ fmap (wrapFile "bib") eFi
  let efd = attachPromptlyDyn (dEnv ^. defXhrRequest) efd1
  r <- performRequestAsync $ ffor efd $ \(defXhr,fd) ->
        xhrRequest "POST" (mappend serverBackend $ T.pack $ show $ linkURI $ jsonApiPutFile) $ defXhr {
                          _xhrRequestConfig_sendData = fd
                         }
  st :: Dynamic t [(T.Text,T.Text)] <- holdDyn [] $ fforMaybe r decodeXhrResponse
  el "p" $ do
    text "Upload status:"
    dynText $ fmap (T.pack . show) st

  {-
  username <- textInput def
  password <- textInput def

  eSend <- toButton "button" (constDyn mempty) $ text "Submit"
  let genLoginReq u p =
        mappend serverBackend $ "auth/signin?login=" <> u <> "&password=" <> p
  let dLoginReq = pure genLoginReq <*> value username <*> value password
  eToken :: Event t (Maybe Token) <- getAndDecode $ tag (current dLoginReq) eSend
  dToken <- holdDyn Nothing eToken
  let dEnv2 = dEnv & auth .~ dToken
  -}
  e <- toButton "button" (constDyn mempty) $ text "Back"
  return ("ImportPage", homePage dEnv <$ e)

loginPage :: (MonadWidget t m) => (Env t) -> Workflow t m T.Text
loginPage dEnv = Workflow . el "div" $ do
  el "div" $ text "LoginPage"
  display (dEnv ^. auth)
  username <- textInput def
  password <- textInput def

  eSend <- button "Submit"
  let genLoginReq u p =
        mappend serverBackend $ "auth/signin?login=" <> u <> "&password=" <> p
  let dLoginReq = pure genLoginReq <*> value username <*> value password
  eToken :: Event t (Maybe Token) <- getAndDecode $ tag (current dLoginReq) eSend
  let (eNewXhr :: Event t (XhrRequestConfig ())) = attachWith putEnvToken (current $ dEnv ^. defXhrRequest) eToken
  dNewXhr <- holdDyn def eNewXhr
  dToken <- holdDyn Nothing eToken
  let dEnv2 = dEnv & auth .~ dToken
                   & defXhrRequest .~ dNewXhr
  e <- button "Back"
  return ("LoginPage", homePage dEnv2 <$ e)
  where
    putEnvToken :: XhrRequestConfig () -> Maybe Token -> XhrRequestConfig ()
    putEnvToken d Nothing = d
    putEnvToken d (Just t) = d { _xhrRequestConfig_headers = Map.singleton "Authorization" $ token t }

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

    let tGetList = mappend serverBackend $ T.pack $ show $ linkURI $ jsonApiGetList 1
    eRefList :: Event t (Maybe [SimpleRef]) <- getAndDecode $ (tGetList <$ eStart)
    dRefList <- holdDyn Nothing eRefList
    --display dRefList
    --eAbstract <- toButton "button" (constDyn mempty) $ text "Abstract"

    delEdit <- flip simpleList dViewArticle $ fromMaybe [] <$> dRefList
    let deEdit = fmap leftmost delEdit
        eEdit = switchDyn deEdit
    dEdit <- holdDyn 0 eEdit
    let thePage = leftmost $ [detailPage dEnv dEdit <$ eEdit,homePage dEnv <$ eHome, loginPage dEnv <$ eLogin, importPage dEnv <$ eImport, noPage dEnv <$ eNav]
    return ("HomePage", thePage)

detailPage :: (MonadWidget t m) => (Env t) -> Dynamic t Int -> Workflow t m T.Text
detailPage dEnv dSerial = Workflow . el "div" $ do
  display (dEnv ^. auth)
  eBack <- toButton "div" mempty $ text "Back"
  el "div" $ text "You have arrived on page 3"
  let tGetSingle = (mappend serverBackend) . T.pack . show . linkURI . jsonApiGetSingle

  eStart <- getPostBuild
  display $ tGetSingle <$> dSerial
  eRef1 :: Event t (Maybe Reference) <- getAndDecode $
    tGetSingle <$> tag (current dSerial) eStart
  let eRef = mapMaybe id eRef1
  dRefs <- holdDyn [] $ (:[]) <$> eRef
  --let dRef = headBlank <$> dRefs
  --eTitle <- inputElement $ def & inputElementConfig_setValue .~ (referenceTitle <$> eRef)
  --dynText $ value eTitle
  dTitle <- holdDyn "TITLE_BLANK" $ referenceTitle <$> mapMaybe id eRef1
  eTitle <- editInPlace (constant True) dTitle -- $ (referenceTitle <$> dRef)
--  display =<< holdDyn "eTitleHold" eTitle

  el "hr" blank
  display =<< holdDyn "eTitleHold" eTitle
  el "hr" blank
  display dRefs
  return ("DetailPage", homePage dEnv <$ eBack)
  where
    buildPostEdit serial f c = XhrRequest "POST" (mappend serverBackend $ T.pack $ show $ linkURI $ jsonApiPutSingleField serial f c)
