{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}
module Api where

import           Servant

import           Action
import           Model
import           Routing

import Servant.API
import Servant.API.Auth.Token

type ExampleAPI = "test"
  :> TokenHeader' '["test-permission"]
  :> Get '[JSON] [SimpleRef]


type Api = AuthAPI :<|> ExampleAPI
  :<|> JsonApi :<|> StaticApi {- :<|> IsomorphicApi -}

  {-
type IsomorphicApi = ToServerRoutes Route HtmlPage Action
-}

type StaticApi = "static" :> Raw

api :: Proxy Api
api = Proxy

exampleApi :: Proxy ExampleAPI
exampleApi = Proxy

authApi :: Proxy AuthAPI
authApi = Proxy

