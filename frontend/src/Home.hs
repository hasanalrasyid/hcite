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
import Types
import Utils
import Home.Detail
import Import
import Login

--import Control.Monad.Reader
import Control.Lens
import Control.Applicative
import Data.Default
import Reflex.Dom.Contrib.Widgets.EditInPlace (editInPlace)
import JSDOM.FormData as FD
import JSDOM.Types (File,MonadJSM)
import Reflex.Dom.Contrib.Widgets.CheckboxList (genCheckbox)

homePage :: MonadWidget t m => (Env t) -> Workflow t m T.Text
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

    let thePage = leftmost $ [ detailPage dEnv dEdit <$ eEdit
                             , homePage dEnv         <$ eHome
                             , loginPage dEnv        <$ eLogin
                             , importPage dEnv       <$ eImport
                             , noPage dEnv           <$ eNav]
    return ("HomePage", thePage)
    where
      genSearchReq (o,(m,s)) =
        let target = case m of
                       SOwner -> textFromJsonApi $ jsonApiGetListOwnerId 1 o
                       _ -> textFromJsonApi $ jsonApiGetListSearch 1
         in postJson target $ Model.Search m s

importPage dEnv = Workflow . el "div" $ do
  e <- importPageWidget dEnv
  return ("importPage", homePage dEnv <$ e)

loginPage dEnv = Workflow . el "div" $ do
  e <- loginPageWidget dEnv
  return ("loginPage", homePage dEnv <$ e)

detailPage dEnv dEdit = Workflow . el "div" $ do
  e <- detailPageWidget dEnv dEdit
  return ("detailPage", homePage dEnv <$ e)

noPage dEnv = Workflow . el "div" $ do
  e <- noPageWidget dEnv
  return ("noPage", homePage dEnv <$ e)
