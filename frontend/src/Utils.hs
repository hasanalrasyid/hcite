{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Utils where

import           Reflex.Dom hiding (Home)
import qualified Data.Text as T
import qualified Data.Map.Strict as Map
import           Data.Monoid ((<>))
--import           Data.FileEmbed

--import Data.Witherable

--import Reflex.Dom.Xhr

--import Language.Javascript.JSaddle.Types
--import Control.Monad.IO.Class

--import Servant.Links

--import Reflex.Bulmex.Tag.Bulma

hiddenDynAttrs :: Map.Map T.Text T.Text -> Bool -> Map.Map T.Text T.Text
hiddenDynAttrs initAttrs b =
  Map.adjust (<> isActive b)  "class" initAttrs
    where isActive True = " is-hidden"
          isActive _    = ""

--activateDynAttrs :: MonadWidget t m => Map.Map T.Text T.Text -> Bool -> Map.Map T.Text T.Text
activateDynAttrs :: Map.Map T.Text T.Text -> Bool -> Map.Map T.Text T.Text
activateDynAttrs initAttrs b =
  Map.adjust (<> isActive b)  "class" initAttrs
    where isActive True = " is-active"
          isActive _    = ""

toButton :: MonadWidget t m => T.Text -> Dynamic t (Map.Map T.Text T.Text) -> m a -> m (Event t ())
toButton d a t = do
  (e,_) <- elDynAttr' d a t
  return $ domEvent Click e





-- | Helper function to create a dynamic attribute map for the visibility of an element
visible :: Eq p => p -> p -> Map.Map T.Text T.Text
visible p1 p2 = "style" =: ("display: " <> choose (p1 == p2) "inline" "none")
  where
    choose True  t _ = t
    choose False _ f = f
