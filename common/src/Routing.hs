{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}
module Routing where

import           Data.Proxy
import           Servant.API
import           Servant.Utils.Links

import           Action
import           Model
import qualified Data.Text as T

--type TopRoute = View Action

type JsonApi =
         "api" :> "record" :> "list" :> Capture "page" Int :>  Get '[JSON] [SimpleRef]
    :<|> "api" :> "record" :> "abs" :> Capture "ident" Int :> Get '[JSON] Abstract
    :<|> "api" :> "record" :> Capture "ident" Int :> Get '[JSON] Reference

type GuardedJsonApi =
         "api" :> "record" :> Capture "ident" Int
            :> ReqBody '[JSON] Reference :> Put '[JSON] NoContent
    :<|> "api" :> "record" :> Capture "ident" Int
            :> Capture "field" T.Text :> Capture "content" T.Text
            :> Put '[JSON] NoContent

(jsonApiGetList :<|> jsonApiGetAbstract :<|> jsonApiGetSingle) = allLinks (Proxy :: Proxy JsonApi)


(jsonApiPutSingle :<|> jsonApiPutSingleField) = allLinks (Proxy :: Proxy GuardedJsonApi)

  {-
listLink :: URI
listLink = linkURI $ safeLink (Proxy :: Proxy Route) (Proxy :: Proxy TopRoute)
-}

