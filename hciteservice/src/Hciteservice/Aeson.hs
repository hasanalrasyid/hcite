module Hciteservice.Aeson where

import           Data.Aeson        (Options)
import           Data.Aeson.Casing (aesonDrop, snakeCase)

defaultHciteserviceOptions :: String -> Options
defaultHciteserviceOptions prefix = aesonDrop (length prefix) snakeCase
