{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}
module Routing where

import           Data.Proxy
import           Servant.API
import           Servant.Links

import           Model
import qualified Data.Text as T
import Servant.Multipart

type JsonApi =
        "api" :> "person" :> ReqBody '[JSON] Model.Search :> Post '[JSON] [Person]
  :<|>  "api" :> "record" :> (
           "abs" :> Capture "ident" Int :> Get '[JSON] Abstract
      :<|> Capture "ident" Int :> Get '[JSON] Reference
      :<|> "list" :> Capture "page" Int :> ReqBody '[JSON] Model.Search :> Post '[JSON] [SimpleRef]
    )

jsonApiGetPerson :: Link
jsonApiGetAbstract :: Int -> Link
jsonApiGetSingle :: Int -> Link
jsonApiGetListSearch :: Int -> Link

( jsonApiGetPerson :<|>
  jsonApiGetAbstract :<|> jsonApiGetSingle
  :<|> jsonApiGetListSearch
  ) = allLinks (Proxy :: Proxy JsonApi)

type GuardedJsonApi =
    "api" :> "record" :> (
             Capture "ident" Int :> ReqBody '[JSON] Reference :> Put '[JSON] NoContent
        :<|> Capture "ident" Int :> Capture "field" T.Text :> Capture "content" T.Text :> Put '[JSON] NoContent
        :<|> MultipartForm Mem (MultipartData Mem) :> Post '[JSON] [(T.Text,T.Text)]
        :<|> Options '[JSON] NoContent
        :<|> "own" :> ReqBody '[JSON] OwnerLRef :> Put '[JSON] NoContent
        :<|> "own" :> ReqBody '[JSON] OwnerLRef :> Delete '[JSON] NoContent
     )
type Options = Verb 'OPTIONS 200

jsonApiPutSingle :: Int -> Link
jsonApiPutSingleField :: Int -> T.Text -> T.Text -> Link
jsonApiPutFile :: Link
jsonApiOptions :: Link
jsonApiPutOwner :: Link
jsonApiDeleteOwner :: Link

(jsonApiPutSingle :<|> jsonApiPutSingleField :<|> jsonApiPutFile :<|> jsonApiOptions
  :<|> jsonApiPutOwner :<|> jsonApiDeleteOwner) = allLinks (Proxy :: Proxy GuardedJsonApi)

