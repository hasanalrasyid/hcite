{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}

module Home where
import           Reflex.Dom hiding (Home)
import qualified Data.Text as T
import qualified Data.Map.Strict as Map
import           Data.Maybe (fromMaybe)

import Routing
import Model

import Navigation
import Types
import Utils
import Home.Detail
import Import
import Login

import Reflex.Dom.Contrib.Widgets.CheckboxList (genCheckbox)

import Storage.Example
import Reflex.Dom.Storage.Class

homePage :: (HasStorage t ExampleTag m, MonadWidget t m) => Workflow t m T.Text
homePage = Workflow $ do
  eNav <- bodyNav
  let eHome = ffilter (== Home) eNav
  let eLogin = ffilter (== Login) eNav
  let eImport = ffilter (== Import) eNav
  el "div" $ mdo
    text "home"
    eStart <- getPostBuild

    drModel <- dropdown SAbstract
                        (constDyn $ Map.fromList [(SAbstract,"abstract")
                                                 ,(SAuthor,"author")
                                                 ,(SKeywords,"keywords")
                                                 ,(SOwner,"owner")])
                        def

    tOwnerSearch <- inputElement def
    eSearchButton <- toButton "button" mempty $ text "Search"

    eNPage <- flip mapM [1..5] $ \iPage -> mdo
                toButton "button" mempty $ text $ T.pack $ show iPage
    dNPage <- holdDyn 1 $ leftmost $ zipWith (<$) [1..] eNPage
    el "br" blank
    display dNPage
    el "br" blank
    let eSearch = leftmost $ eNPage ++ [eSearchButton, eStart]
    let ePostXhrRequest = fmap genSearchReq
                              $ attachPromptlyDyn dNPage
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
    dAuth <- askStorageTagDef Tag1 Nothing
    display dAuth
    bulkExecute dAuth dTOwner (fmap (filter snd) dBulk) $ tag (current $ _dropdown_value dropdownBulkAction) eBulkExecute

    bulkAll <- genCheckbox text "CheckAll" $ _inputElement_checkedChange bulkAll
    text "bulkAll"
    display $ _inputElement_checked bulkAll
    display dBulk

    dleEdit <- flip simpleList (dViewArticle (_inputElement_checkedChange bulkAll)) dRefListSearch
    let dBulk = fmap (map snd . Map.toList) $ joinDynThroughMap $ fmap (\x -> Map.fromList $ zip ([1..] :: [Int]) $ map fst x) dleEdit
    let deEdit = fmap (leftmost . (map snd)) dleEdit
        eEdit = switchDyn deEdit
    dEdit <- holdDyn 0 eEdit

    let thePage = leftmost $ [ detailPage dEdit <$ eEdit
                             , homePage <$ eHome
                             , loginPage <$ eLogin
                             , importPage <$ eImport
                             , noPage <$ eNav]
    return ("HomePage", thePage)
    where
      genSearchReq (nPage,(o,(m,s))) =
        let target = case m of
                       SOwner -> textFromJsonApi $ jsonApiGetListOwnerId nPage o
                       _ -> textFromJsonApi $ jsonApiGetListSearch nPage
         in postJson target $ Model.Search m s

homeWidget :: (HasStorage t ExampleTag m, MonadWidget t m) => m (Event t T.Text)
homeWidget = do
  r <- workflow homePage
  display r
  return $ updated r

importPage :: (HasStorage t ExampleTag m, MonadWidget t m) => Workflow t m T.Text
importPage = Workflow . el "div" $ do
  e <- importPageWidget
  return ("importPage", homePage <$ e)

loginPage :: (HasStorage t ExampleTag m, MonadWidget t m) => Workflow t m T.Text
loginPage = Workflow . el "div" $ mdo
  eEnvNew <- loginPageWidget
  return ("loginPage", homePage <$ eEnvNew)

detailPage :: (HasStorage t ExampleTag m, MonadWidget t m) => Dynamic t Int -> Workflow t m T.Text
detailPage dEdit = Workflow . el "div" $ do
  e <- detailPageWidget dEdit
  return ("detailPage", homePage <$ e)

noPage :: (HasStorage t ExampleTag m, MonadWidget t m) => Workflow t m T.Text
noPage = Workflow . el "div" $ do
  e <- noPageWidget
  return ("noPage", homePage <$ e)
