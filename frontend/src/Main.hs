{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}

import           Reflex.Dom hiding (Home)
import qualified Data.Map.Strict as Map

import Home

import Reflex.Dom.Storage.Base
import Reflex.Dom.Storage.Class

import Control.Monad (void)
import Storage.Example (ExampleTag(..))

main :: IO ()
main = mainWidgetWithHead headElement $ mdo
  void . runStorageT LocalStorage $ do
    initializeTag Tag1 Nothing
    body

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

body :: (HasStorage t ExampleTag m, MonadWidget t m) => m ()
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

