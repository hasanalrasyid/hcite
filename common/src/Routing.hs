{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}
module Routing where

import           Data.Proxy
import           Servant.API
import           Servant.Utils.Links

import           Action
import           Model
import qualified Data.Text as T
import Servant.Multipart

--type TopRoute = View Action

type JsonApi =
    "api" :> "record" :> (
           "abs" :> Capture "ident" Int :> Get '[JSON] Abstract
      :<|> Capture "ident" Int :> Get '[JSON] Reference
      :<|> "list" :> (
                   Capture "page" Int :>  Get '[JSON] [SimpleRef]
              :<|> "author" :> Capture "search" T.Text :> Capture "page" Int :>  Get '[JSON] [SimpleRef]
              :<|> "abstract" :> Capture "search" T.Text :> Capture "page" Int :>  Get '[JSON] [SimpleRef]
              :<|> "keywords" :> Capture "search" T.Text :> Capture "page" Int :>  Get '[JSON] [SimpleRef]
              :<|> "owner" :> Capture "search" Int :> Capture "page" Int :>  Get '[JSON] [SimpleRef]
            )
    )

( jsonApiGetAbstract :<|> jsonApiGetSingle
  :<|> jsonApiGetList
  :<|> jsonApiGetListAuthor
  :<|> jsonApiGetListAbstract
  :<|> jsonApiGetListKeyword
  :<|> jsonApiGetListOwnerId ) = allLinks (Proxy :: Proxy JsonApi)

type GuardedJsonApi =
    "api" :> "record" :> (
             Capture "ident" Int :> ReqBody '[JSON] Reference :> Put '[JSON] NoContent
        :<|> Capture "ident" Int :> Capture "field" T.Text :> Capture "content" T.Text :> Put '[JSON] NoContent
        :<|> MultipartForm Mem (MultipartData Mem) :> Post '[JSON] [(T.Text,T.Text)]
        :<|> Options '[JSON] NoContent
     )
type Options = Verb 'OPTIONS 200

(jsonApiPutSingle :<|> jsonApiPutSingleField :<|> jsonApiPutFile :<|> jsonApiOptions) = allLinks (Proxy :: Proxy GuardedJsonApi)

  {-
listLink :: URI
listLink = linkURI $ safeLink (Proxy :: Proxy Route) (Proxy :: Proxy TopRoute)
-}

