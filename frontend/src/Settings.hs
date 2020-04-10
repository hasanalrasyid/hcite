{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Settings where

import qualified Data.Text as T



serverBackend :: T.Text
serverBackend = "http://127.0.0.1:3000/"
--serverBackend = "http://192.168.43.175:3000/"

