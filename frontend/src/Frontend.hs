{-|
Copyright   : (c) 2018, Commonwealth Scientific and Industrial Research Organisation
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}
module Frontend where

import Data.Functor (void)
--import Obelisk.Frontend
--import Obelisk.Route
import Reflex.Dom.Core

import Control.Lens

--import Common.Route

import Reflex.Dom.Storage.Base
import Reflex.Dom.Storage.Class
import Storage.Example

import Types
import Model

  {-
frontend :: Frontend (R FrontendRoute)
frontend = Frontend
  { _frontend_head = el "title" $ text "Obelisk Minimal Example"
  , _frontend_body = prerender_ (text "Loading...") $ do
    text "Testing storage"
    void . runStorageT LocalStorage $ do
      initializeTag Tag1 0
      counter
      foo
  }
-}
foo :: (MonadWidget t m, HasStorage t ExampleTag m) => m ()
foo = el "div" $ do
  dTag2 <- askStorageTagDef Tag2 $ Foo False "Hi"
  dBool <- holdUniqDyn $ bar <$> dTag2
  iBool <- sample . current $ dBool
  let eBool = updated dBool
  cb <- checkbox iBool $ def & setValue .~ eBool

  tellStorageInsert Tag2 $ (\x b -> x { bar = b})  <$> current dTag2 <@> (cb ^. checkbox_change)

counter :: (MonadWidget t m, HasStorage t ExampleTag m) => m ()
counter = el "div" $ do
  dTag1 <- askStorageTagDef Tag1 Nothing

  eAdd <- button "Add"
  eClear <- button "Clear"
  display dTag1

  let
    {-
    eChange = mergeWith (.) [
        succ <$ eAdd
      , const 0 <$ eClear
      ]
-}
    dNewTag1 = Just (Token "newTag") <$ dTag1
    eChange = Just "new one" <$ eClear
  tellStorageInsert Tag1 $ tag (current dNewTag1) eChange
