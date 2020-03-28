module Server(
  -- * Server config
    ServerConfig(..)
  , readConfig
  -- * Server environment
  , ServerEnv
  , newServerEnv
  -- * Execution of server
  , exampleServerApp
  , runExampleServer
  ) where

import Control.Monad.IO.Class
import Control.Monad.Logger
import Data.Proxy
import Network.Wai
import Network.Wai.Handler.Warp
import Network.Wai.Middleware.RequestLogger
import Servant.API.Auth.Token
import Servant.Server
import Servant.Server.Auth.Token

import Api
import Config
import qualified Monad as M
import Monad hiding (newServerEnv)
import Auth.Monad

import Servant
import qualified Data.Map as Map
--import qualified Lucid as L

import Action
import Model
import Routing
--import View
--import Record.List
--import Record.Edit

--import Html

import Network.HTTP.Types

import Database.Persist

import Network.Wai.Middleware.Cors

-- | Enter infinite loop of processing requests for pdf-master-server.
--
-- Starts new Warp server with initialised threads for serving the master server.
runExampleServer :: MonadIO m => ServerConfig -> m ()
runExampleServer config = liftIO $ do
  env <- M.newServerEnv config
  liftIO $ run (serverPort config) $ logStdoutDev $ exampleServerApp env

-- | WAI application of server
exampleServerApp :: ServerEnv -> Application
exampleServerApp e = simpleCors $ serve api myImpl
  where
    myImpl = authImpl e :<|> exampleImpl e
                        :<|> jsonImpl e
                        :<|> staticFiles
                          {-
                        :<|> ssrViews
--                        :<|> notFoundHtml
--                        -}

  {-
notFoundHtml :: Application
notFoundHtml _ respond =
    respond $ responseLBS
        status404 [("Content-Type", "text/html1")] $
        L.renderBS $ L.toHtml (HtmlPage notFoundPage)
-}

exampleImpl e = hoistServer exampleApi (runServerM e) exampleServer

authImpl e = hoistServer authApi (runAuthM e) authServerM

jsonImpl e = getRecords e :<|> getAbstract e :<|> getRecord e :<|> putRecordById e :<|> putRecordFieldById e

getRecord e i = do
  p <- withDB e $ selectList [ ReferenceSerial ==. i ] []
  return $ entityVal $ head p

getAbstract e i = fromReference <$> getRecord e i

resPerPage = 5

getRecords e iPage = do
  p <- withDB e $ selectList [] [ LimitTo resPerPage
                                , OffsetBy $ (iPage - 1) * resPerPage ]
  return $ map (fromReference . entityVal) p


putRecordById e i p = do
  withDB e $ do
    p0 <- selectList [ ReferenceSerial ==. i ] [LimitTo 1]
    repsert (entityKey $ head p0) p
  return NoContent

putRecordFieldById e i f c = do
  return NoContent

-- | Implementation of main server API
exampleServer :: ServerT ExampleAPI ServerM
exampleServer = testEndpoint

testEndpoint :: MToken' '["test-permission"] -> ServerM ()
testEndpoint token = do
  runAuth $ guardAuthToken token
  liftIO $ putStrLn "testEndpoint"
  return ()

staticFiles = serveDirectoryFileServer "static"

  {-
ssrViews :: Server IsomorphicApi
ssrViews = topView

topView :: Handler (HtmlPage (View Action))
topView = return $ HtmlPage . viewModel . initialModel $ listLink

listView :: Handler (HtmlPage (View Action))
listView = topView
-}

