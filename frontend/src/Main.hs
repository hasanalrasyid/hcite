{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

import           Reflex.Dom hiding (Home)
import qualified Data.Text as T
import qualified Data.Map.Strict as Map
import           Data.Maybe --(fromJust,fromMaybe)
import           Data.Monoid ((<>))
--import           Data.FileEmbed

--import Data.Witherable

import Common
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

-- A DOM based example of Workflow
page1, page2, page3 :: (MonadWidget t m) => Workflow t m T.Text
page1 = Workflow . el "div" $ do
  el "div" $ text "This is page 1"
  pg2 <- button "Switch to page 2"
  return ("Page 1", page2 <$ pg2)

page2 = Workflow . el "div" $ do
  el "div" $ text "This is page 2"
  pg3 <- button "Switch to page 3"
  pg1 <- button "No wait, I want to go back to page 1"
  return ("Page 2", leftmost [page3 <$ pg3, page1 <$ pg1])

page3 = Workflow . el "div" $ do
  el "div" $ text "You have arrived on page 3"
  pg1 <- button "Start over"
  return ("Page 3", page1 <$ pg1)

textWidget,unimplementedWidget :: MonadWidget t m => m (Event t T.Text)
textWidget = mdo
  eClick <- toButton "div" (constDyn ("class" =: "navbar-item")) $
    text "textWidget"
  return ("textWidget" <$ eClick)


unimplementedWidget = mdo
  eClick <- toButton "div" (constDyn ("class" =: "navbar-item")) $
    text "Unimplemented"
  return ("Unimplemented" <$ eClick)

  {-
detailWidget dDetail = mdo
  text "detailWidget"
  el "hr" blank
  display dDetail
  eStart <- getPostBuild
  let tGetSingle = (mappend serverBackend) . T.pack . show . linkURI . jsonApiGetSingle
  eRef :: Event t (Maybe Reference) <- getAndDecode $ updated $ tGetSingle <$> dDetail
  display =<< holdDyn Nothing eRef
  eEdit <- toButton "button" (constDyn mempty) $ text "Send"
  return ( 0 <$ eEdit)
-}

homeWidget :: MonadWidget t m => m (Event t T.Text)
homeWidget = mdo
  r <- workflow homePage
  display r
  return $ updated r


detailPage :: (MonadWidget t m) => Dynamic t Int -> Workflow t m T.Text
detailPage dSerial = Workflow . el "div" $ do
  el "div" $ text "You have arrived on page 3"
  let tGetSingle = (mappend serverBackend) . T.pack . show . linkURI . jsonApiGetSingle

  eStart <- getPostBuild
  display $ tGetSingle <$> dSerial
  eRef :: Event t (Maybe Reference) <- getAndDecode $
    tGetSingle <$> tag (current dSerial) eStart
  display =<< holdDyn Nothing eRef
  pg1 <- button "Back"
  return ("DetailPage", homePage <$ pg1)
homePage :: (MonadWidget t m) => Workflow t m T.Text
homePage = Workflow . el "div" $ mdo
  text "home"
  eStart <- getPostBuild
  let tGetList = mappend serverBackend $ T.pack $ show $ linkURI $ jsonApiGetList 1
  eRefList :: Event t (Maybe [SimpleRef]) <- getAndDecode $ (tGetList <$ eStart)
  dRefList <- holdDyn Nothing eRefList
  display dRefList
  eAbstract <- toButton "button" (constDyn mempty) $ text "Abstract"
  eEdit <- toButton "button" (constDyn mempty) $ text "Edit"
  dEdit <- holdDyn 0 $ 22 <$ eEdit
  return ("HomePage", detailPage dEdit <$ eEdit)


body :: MonadWidget t m => m ()
body = mdo
  eNav :: Event t Nav <- bodyNav
  let eHome = ffilter (== Home) eNav
      eUnimplemented = ffilter (/= Home) eNav
      eCurrent = leftmost [Right <$> eHome, Left <$> eUnimplemented]

  dUnimplemented <- holdDyn Nothing $ fmap Just eUnimplemented
  dCurrent <- holdDyn (Left Home) $ eCurrent

  dNav <- holdDyn Home eNav
  display dNav

  -- Builds up a `Dynamic` of widgets that return `Event t Text`:
  dWidget <- holdDyn homeWidget . leftmost $ [
      homeWidget <$ eHome
    , unimplementedWidget <$ eUnimplemented
    ]

-- di sini, textWidget dan textWidget2 return Event t Int from referenceSerial.
-- Sebab isi textWidget* adalah:
-- 1. HomePage
-- 2. LoginPage
-- 3. DetailPage (bisa edited bisa static)
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
             , "eSwitch" <$ eNav
             ]

  el "div" $
    dynText dText

  {-

  r <- workflow page1
  el "div" $ do
    text "Current page is: "
    dynText r
-}
