{-# LANGUAGE ScopedTypeVariables      #-}

module Reflex.Dom.Contrib.Widgets.CheckboxList where

------------------------------------------------------------------------------
import           Control.Monad
import           Data.Set (Set)
import qualified Data.Set as S
import           Reflex
import           Reflex.Dom
import qualified Data.Map as Map
import qualified Data.Text as T
------------------------------------------------------------------------------
------------------------------------------------------------------------------
-- | Takes a list of labels to make checkboxes for and returns the labels of
-- the boxes that are checked.
checkboxList
    :: (MonadWidget t m, Ord a, Show a)
    => (a -> T.Text)
    -- ^ Function to show each item
--      (T.Text -> a -> Bool)
    -- ^ Function to filter each item
    -> Event t Bool
    -- ^ Blanket event to apply to all list items.  Allows you to have "select
    -- all" and "select none" buttons.  Fire True to select all and False to
    -- select none.
--    -> Dynamic t T.Text
    -- ^ A search string for filtering the list of items.
    -> Set a
    -- ^ Set of items that should be initially checked
    -> [a]
    -- ^ List of items to show checkboxes for
    -> m (Dynamic t [a])
    -- ^ Dynamic list of checked items
--checkboxList showFunc filterFunc blanketEvent searchString onItems items = do
checkboxList showFunc blanketEvent onItems items = do
    el "ul" $ do
      es <- forM items $ \item -> do
        let shown = showFunc item
        {-
            mkAttrs search =
              if filterFunc search item
                then mempty
                else "style" =: "display:none"
        --attrs <- liftM unqDyn (mapDyn mkAttrs searchString)
        ---}
        let attrs = constDyn mempty

        elDynAttr "li" attrs $ el "label" $ do
          cb <- htmlCheckbox
                  (leftmost [blanketEvent])
                  (S.member item onItems :: Bool)
--                  (constDyn mempty)
          text shown
          return $ fmap (\b -> if b then [item] else []) $ _inputElement_checked cb
      pure $ fmap concat $ distributeListOverDyn es

--htmlCheckbox setV initV attr = do
htmlCheckbox setV initV = do
    elClass "label" "checkbox" $ do
      cb <- inputElement $ def & inputElementConfig_setChecked .~ setV
                               & initialAttributes .~
                                    Map.fromList [ ("class", "is-checkradio is-block")
                                                 , ("type", "checkbox")
                                                 ]

          {- initV $ def
        & setValue .~ setV
        & attributes .~ attr
        -}
      return cb

mapCheckbox f (Checkbox v c) = do
  return $ fmap f v


mapDyn f x = do
  return $ map f x


  {-
------------------------------------------------------------------------------
-- | Takes a list of labels to make checkboxes for and returns the labels of
-- the boxes that are checked.
checkboxListView
    :: forall t m a b. (MonadWidget t m, Ord a, Show a)
    => (a -> String)
    -- ^ Function to show each item
    -> (String -> a -> Bool)
    -- ^ Function to filter each item
    -> (a -> Bool -> b)
    -> Event t Bool
    -- ^ Blanket event to apply to all list items.  Allows you to have "select
    -- all" and "select none" buttons.  Fire True to select all and False to
    -- select none.
    -> Dynamic t String
    -- ^ A search string for filtering the list of items.
    -> Set a
    -- ^ Set of items that should be initially checked
    -> [a]
    -- ^ List of items to show checkboxes for
    -> m (Event t b)
    -- ^ Events changing the selected set
checkboxListView showFunc filterFunc updateFunc blanketEvent searchString
                 onItems items = do
    el "ul" $ do
      es <- forM items $ \item -> do
        let shown = showFunc item
            mkAttrs search =
              if filterFunc search item
                then mempty
                else "style" =: "display:none"
        attrs <- liftM nubDyn $ mapDyn mkAttrs searchString
        elDynAttr "li" attrs $ el "label" $ do
          cb <- htmlCheckbox $ WidgetConfig
                  (leftmost [blanketEvent])
                  (S.member item onItems)
                  (constDyn mempty)
          text shown
          return $ updateFunc item <$> _hwidget_change cb
      return $ leftmost es
-}
