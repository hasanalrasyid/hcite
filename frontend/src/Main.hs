{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

import           Reflex.Dom hiding (Home)
import qualified Data.Text as T
import qualified Data.Map.Strict as Map
import           Data.Maybe --(fromJust,fromMaybe)
--import           Data.FileEmbed

--import Data.Witherable

import Routing
import Model

--import Reflex.Dom.Xhr

--import Language.Javascript.JSaddle.Types
--import Control.Monad.IO.Class
import Control.Monad
import Control.Monad.Trans

import Servant.Links

import Reflex.Bulmex.Modal
import Reflex.Bulmex.Tag.Bulma

import Proto hiding (main,headElement,body)
import Home  hiding (main,headElement,body,homePage,homeWidget,detailPage)

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
-- 2. LoginPage
-- v. DetailPage (bisa static)
-- 3. DetailPage (bisa edited)
-- 4. DetailedSearchPage
-- 5. SearchResultPage (seperti HomePage tapi dengan content yang berbeda)
-- 6. ImportPage (ini yang paling penting sebenarnya)
--
--

  -- Using `dyn` on this gives us an `Event t (Event t Text)`:
  eeDetail <- dyn dWidget
  -- and we can use `switchHold` to turn that into an `Event t Int`:
  eDetail <- switchHold never eeDetail
  dDetail <- holdDyn "" eDetail

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
  r <- workflow homePage
  display r
  return $ updated r

noPage :: (MonadWidget t m) => Workflow t m T.Text
noPage = Workflow . el "div" $ do
  el "div" $ text "No Page So Far"
  e <- button "Home"
  return ("noPage", homePage <$ e)

homePage :: (MonadWidget t m) => Workflow t m T.Text
homePage = Workflow $ do
  eNav <- bodyNav
  let eHome = ffilter (== Home) eNav
  el "div" $ mdo
    text "home"
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
    let thePage = leftmost $ [detailPage dEdit <$ eEdit,homePage <$ eHome, noPage <$ eNav]
    return ("HomePage", thePage)
    --return ("HomePage", detailPage dEdit <$ eEdit)


detailPage :: (MonadWidget t m) => Dynamic t Int -> Workflow t m T.Text
detailPage dSerial = Workflow . el "div" $ do
  el "div" $ text "You have arrived on page 3"
  let tGetSingle = (mappend serverBackend) . T.pack . show . linkURI . jsonApiGetSingle

  eStart <- getPostBuild
  display $ tGetSingle <$> dSerial
  eRef :: Event t (Maybe Reference) <- getAndDecode $
    tGetSingle <$> tag (current dSerial) eStart
  display =<< holdDyn Nothing eRef
  pg1 <- toButton "div" mempty $ text "Back"
  return ("DetailPage", homePage <$ pg1)

