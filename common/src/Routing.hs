{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}
module Routing where

import           Data.Proxy
import           Servant.API
import           Servant.Utils.Links

import           Action
import           Model


--type TopRoute = View Action

type JsonApi =
         "api" :> "record" :> "list" :> Capture "page" Int :>  Get '[JSON] [SimpleRef]
    :<|> "api" :> "record" :> Capture "ident" Int :> Get '[JSON] [Reference]
    :<|> "api" :> "record" :> Capture "ident" Int
            :> ReqBody '[JSON] Reference :> Put '[JSON] NoContent

(jsonApiGetList :<|> jsonApiGetSingle :<|> jsonApiPutSingle) = allLinks (Proxy :: Proxy JsonApi)

  {-
listLink :: URI
listLink = linkURI $ safeLink (Proxy :: Proxy Route) (Proxy :: Proxy TopRoute)
-}

