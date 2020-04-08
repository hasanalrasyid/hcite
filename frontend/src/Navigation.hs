{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Navigation where

import           Reflex.Dom hiding (Home)
import qualified Data.Text as T
import           Data.Monoid ((<>))
--import           Data.FileEmbed

--import Data.Witherable


--import Reflex.Dom.Xhr

--import Language.Javascript.JSaddle.Types
--import Control.Monad.IO.Class

import Reflex.Bulmex.Tag.Bulma

import Utils
import Types

showNav :: Nav -> T.Text
showNav = T.pack . show

showNavPage :: NavPage -> T.Text
showNavPage = T.pack . show

navPage :: [T.Text]
navPage = navMenu showNavPage

navMenu :: (Show a, Enum a) => (a -> T.Text) -> [T.Text]
navMenu sn = map sn $ enumFrom $ toEnum 0

navMenuImplemented :: [Nav]
navMenuImplemented = [Home]

bodyNav :: MonadWidget t m => m (Event t Nav)
bodyNav = do
  elClass "nav" "navbar topNav" $ do
    container $ mdo
      dynToggleTopNav <- toggle False $ leftmost [evToggleTopNav, () <$ evNav]
      evToggleTopNav <- elClass "div" "navbar-brand" $ do
        elClass "a" "navbar-item" $ -- href="../">
          el "h1" $ text "HCITE"
          -- elAttr "img" (("src" =: "inc/img/bulma.png") <> ("width" =: "112") <> ("height"=:"28")) blank
        toButton "div" (activateDynAttrs (("class" =: "navbar-burger burger") <> ("data-target" =: "topNav")) <$> dynToggleTopNav) $ do
              el "span" blank
              el "span" blank
              el "span" blank
--        </div>
--      </div>
--    </div>
      evNav <- elDynAttr "div" (activateDynAttrs (( "class" =: "navbar-menu") <> ("id" =: "topNav")) <$> dynToggleTopNav) $ do
        let (navMenuStart,(navMenuRegister:navMenuLogin:_)) = splitAt 7 $ navMenu showNav
        (evNavStart :: [Event t ()]) <- elClass "div" "navbar-start" $ do
          mapM (\t -> toButton "a" (constDyn ("class" =: "navbar-item")) $ text t) $ navMenuStart
          --return $ fmap toEnum $ leftmost $ zipWith (<$) [0..] evNavR0

        (evNavEnd :: [Event t ()]) <- elClass "div" "navbar-end" $ do
          elClass "div" "navbar-item" $ do
            elClass "div" "field is-grouped"$ do
              evNavR0 <- toButton "p" (constDyn $ "class" =: "control") $ do
                elClass "a" "button is-small" $ do
                  elClass "span" "icon" $ do
                    elClass "i" "fa fa-user-plus" blank
                  el "span" $ text navMenuRegister
              evNavR1 <- toButton "p" (constDyn $ "class" =: "control") $ do
                elClass "a" "button is-small is-info is-outlined" $ do
                  elClass "span" "icon" $ do
                    elClass "i" "fa fa-user" blank
                  el "span" $ text navMenuLogin
              return $ [evNavR0,evNavR1]
        return $ fmap toEnum $ leftmost $ zipWith (<$) [0..] $ evNavStart ++ evNavEnd
      return evNav

  {- not needed
  <nav class="navbar is-white">
  <div class="container">
  <div class="navbar-menu">
  <div class="navbar-start">
  <a class="navbar-item is-active" href="#">Popular</a>
  <a class="navbar-item" href="#">Recent</a>
  <a class="navbar-item" href="#">Rising</a>
  </div>
  <div class="navbar-end">
  <div class="navbar-item">
  <input class="input" type="search" placeholder="Search forum...">
  </div>
  </div>
  </div>
  </div>
  </nav>
  -}
