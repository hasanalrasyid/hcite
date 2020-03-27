{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE MultiWayIf                #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE RecursiveDo               #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE TupleSections             #-}
{-# LANGUAGE TypeFamilies              #-}

module Reflex.Dom.Contrib.Widgets.EditInPlace
  ( editInPlace
  ) where

------------------------------------------------------------------------------
import           Control.Monad.Trans
import           Data.Map (Map)
import           Data.Text (Text)
import qualified Data.Text as T
--import           GHCJS.DOM.HTMLElement
import           Reflex
import           Reflex.Dom.Core
import           Reflex.Dom.Contrib.Utils
--import           Reflex.Dom.Contrib.Widgets.Common
--import           GHCJS.DOM.Types (MonadJSM)
import Control.Lens
import Data.Default
------------------------------------------------------------------------------
import JSDOM.Element
import JSDOM.Types (liftDOM,MonadJSM(..))
import Control.Monad (void)
import Language.Javascript.JSaddle (jsf)

------------------------------------------------------------------------------
data EditState = Viewing
               | Editing
  deriving (Eq,Show,Ord,Enum)


------------------------------------------------------------------------------
-- | Control that can be used in place of dynText whenever you also want that
-- text to be editable in place.
--
-- This control creates a span that either holds text or a text input field
-- allowing that text to be edited.  The edit state is activated by clicking
-- on the text.  Edits are saved when the user presses enter or abandoned if
-- the user presses escape or the text input loses focus.
editInPlace
    :: (MonadWidget t m)
    => Behavior t Bool
    -- ^ Whether or not click-to-edit is enabled
    -> Dynamic t Text
    -- ^ The definitive value of the thing being edited
    -> m (Event t Text)
    -- ^ Event that fires when the text is edited
editInPlace active val = do
    rec
        let startEditing = fmapMaybe id $
              (\a -> if a then Just Editing else Nothing) <$> selActive
        editState <- holdDyn Viewing $ leftmost
          [ fmapMaybe id $ attachWith
              (\c n -> if c == Editing then Nothing else Just n)
              (current editState) startEditing
          , Viewing <$ sheetEdit
          ]
        (e, sheetEdit) <- elDynAttr' "span" (mkClass <$> editState) $ do
          de <- widgetHoldHelper (chooser val) Viewing (updated editState)
          return $ switch $ current de
        let selActive = tag active $ domEvent Click e
    return $ fmapMaybe e2maybe sheetEdit

------------------------------------------------------------------------------
mkClass :: EditState -> Map Text Text
mkClass es = "class" =: (T.unwords ["editInPlace", ev])
  where
    ev = case es of
           Viewing -> "viewing"
           Editing -> "editing"


------------------------------------------------------------------------------
e2maybe :: SheetEditEvent -> Maybe Text
e2maybe EditClose = Nothing
e2maybe (NameChange s) = Just s

------------------------------------------------------------------------------
chooser
    :: (MonadWidget t m)
    => Dynamic t Text
    -> EditState
    -> m (Event t SheetEditEvent)
chooser name Editing = editor name
chooser name Viewing = viewer name

------------------------------------------------------------------------------
data SheetEditEvent = NameChange Text
                    | EditClose
  deriving (Read,Show,Eq,Ord)

focus :: (IsElement self, MonadJSM m) => self -> m ()
focus self =
  liftDOM $ void $ (toElement self) ^. jsf ("focus" :: String) ()

------------------------------------------------------------------------------
editor
    :: (MonadWidget t m)
    => Dynamic t Text
    -> m (Event t SheetEditEvent)
editor name = mdo
    {-
  (e,w) <- htmlTextInput' "text" $ WidgetConfig
    (tagPromptlyDyn name pb) "" (constDyn mempty)
  performEvent_ $ ffor pb $ \_ -> do
    liftIO $ focus e
    -}
  w <- inputElement $ def & inputElementConfig_setValue .~ (tagPromptlyDyn name pb)
  pb <- getPostBuild
  performEvent_ $ ffor pb $ \_ -> do
    focus $ _element_raw $ _inputElement_element w
  let eEnter = ffilter (==13) $ domEvent Keypress w
  let eEsc   = ffilter (==27) $ domEvent Keypress w

  let acceptEvent = leftmost
        [ () <$ eEnter -- () <$ ffilter (==13) (_inputElement_input w) ,
        , () <$ ffilter not (updated $ _inputElement_hasFocus w)
        ]
  return $ leftmost
    [ EditClose <$ eEsc
    , NameChange <$> tag (current $ _inputElement_value w) acceptEvent
    --, EditClose <$ ffilter (==27) (_inputElement_input w)
    , EditClose <$ ffilter  not (updated $ _inputElement_hasFocus w)
    ]

------------------------------------------------------------------------------
viewer
    :: MonadWidget t m
    => Dynamic t Text
    -> m (Event t SheetEditEvent)
viewer name = do
  dynText name
  return never

  {-
------------------------------------------------------------------------------
-- | HtmlWidget version of reflex-dom's textInput that also returns the
-- HTMLInputElement.
htmlTextInput'
    :: MonadWidget t m
    => Text
    -> WidgetConfig t Text
    -> m String
htmlTextInput' inputType cfg = do
    ti <- inputElement $ def
      & inputElementConfig_setValue .~ _widgetConfig_setValue cfg
      & inputElementConfig_attributes .~ _widgetConfig_attributes cfg
      & textInputConfig_initialValue .~ _widgetConfig_initialValue cfg
      & textInputConfig_inputType .~ inputType
    let w = TextInput
          (_textInput_value ti)
          (_textInput_input ti)
          (fromIntegral <$> _textInput_keypress ti)
          (fromIntegral <$> _textInput_keydown ti)
          (fromIntegral <$> _textInput_keyup ti)
          (_textInput_hasFocus ti)
    return (_textInput_element ti, w)
-}

------------------------------------------------------------------------------
-- | Generic config structure common to most widgets.  The attributes field
-- may not be used for all widgets, but in that case it can just be ignored.
-- We may want to change this in the future, but it seems like a reasonable
-- start for now.
data WidgetConfig t a
    = WidgetConfig { _widgetConfig_setValue :: Event t a
                   , _widgetConfig_initialValue :: a
                   , _widgetConfig_attributes :: Dynamic t (Map Text Text)
                   }

instance Reflex t => Functor (WidgetConfig t) where
    fmap f (WidgetConfig sv iv a) = WidgetConfig (f <$> sv) (f iv) a


makeLenses ''WidgetConfig

instance (Reflex t, Default a) => Default (WidgetConfig t a) where
  def = WidgetConfig { _widgetConfig_setValue = never
                     , _widgetConfig_initialValue = def
                     , _widgetConfig_attributes = constDyn mempty
                     }

instance HasAttributes (WidgetConfig t a) where
  type Attrs (WidgetConfig t a) = Dynamic t (Map Text Text)
  attributes = widgetConfig_attributes

instance HasSetValue (WidgetConfig t a) where
  type SetValue (WidgetConfig t a) = Event t a
  setValue = widgetConfig_setValue


