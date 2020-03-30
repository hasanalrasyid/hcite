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
--import Control.Monad.Logger
--import Data.Proxy
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
--import qualified Data.Map as Map
--import qualified Lucid as L

--import Action
import Model
import Routing
--import View
--import Record.List
--import Record.Edit

--import Html

import Network.HTTP.Types
import Database.Persist

import Network.Wai.Middleware.Cors
import qualified Data.Text as T

import Data.Maybe (fromMaybe,isNothing)
import Servant.Multipart
--import qualified Data.ByteString.Lazy.Char8 as LBS
import Filler
import Control.Monad
import Data.Either
--import Control.Monad.Reader -- asks

-- | Enter infinite loop of processing requests for pdf-master-server.
--
-- Starts new Warp server with initialised threads for serving the master server.
runExampleServer :: MonadIO m => ServerConfig -> m ()
runExampleServer config = liftIO $ do
  env <- M.newServerEnv config
  liftIO $ run (serverPort config) $ logStdoutDev $ exampleServerApp env

-- | WAI application of server
exampleServerApp :: ServerEnv -> Application
exampleServerApp e = (cors allOK) $ serve api myImpl
--exampleServerApp e = simpleCors $ serve api myImpl
  where
    allOK _ = Just $ CorsResourcePolicy
      { corsOrigins = Nothing
        {-
                    Just (["localhost:3000"
                          ,"127.0.0.1:3000"],True)
                    -}
                , corsMethods = [methodGet,methodPost,methodOptions,methodPut]
                , corsRequestHeaders = ["Access-Control-Request-Method"
                                       ,"Access-Control-Request-Headers"
                                       ,"Authorization"]
                , corsExposedHeaders = Nothing
                , corsMaxAge = Nothing
                , corsVaryOrigin = False
                , corsRequireOrigin = False
                , corsIgnoreFailures = False
                }
    myImpl = authImpl e :<|> exampleImpl e
                        :<|> jsonImpl e
                        :<|> staticFiles
                        :<|> guardedJsonImpl e
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

exampleImpl :: ServerEnv -> Server ExampleAPI
exampleImpl e = hoistServer exampleApi (runServerM e) exampleServer

authImpl :: ServerEnv -> Server AuthAPI
authImpl e = hoistServer authApi (runAuthM e) authServerM

jsonImpl :: ServerEnv -> Server JsonApi
jsonImpl e = getRecords e :<|> getAbstract e :<|> getRecord e

guardedJsonImpl :: ServerEnv -> Server GuardedJsonBackendApi
guardedJsonImpl e =
  hoistServer guardedJsonBackendApi (runServerM e) guardedServer

getRecord :: MonadIO m => ServerEnv -> Int -> m Reference
getRecord e i = do
  p <- withDBEnv e $ selectList [ ReferenceSerial ==. i ] []
  return $ entityVal $ head p

getAbstract :: (FromReference b, MonadIO f) => ServerEnv -> Int -> f b
getAbstract e i = fromReference <$> getRecord e i

resPerPage :: Int
resPerPage = 5

getRecords :: (FromReference b, MonadIO f) => ServerEnv -> Int -> f [b]
getRecords e iPage = do
  p <- withDBEnv e $ selectList [] [ LimitTo resPerPage
                                , OffsetBy $ (iPage - 1) * resPerPage ]
  return $ map (fromReference . entityVal) p

  {-
putRecordById e i p = do
  withDBEnv e $ do
    p0 <- selectList [ ReferenceSerial ==. i ] [LimitTo 1]
    repsert (entityKey $ head p0) p
  return NoContent
-}

-- | Implementation of main server API
exampleServer :: ServerT ExampleAPI ServerM
exampleServer = testEndpoint

guardedServer :: ServerT GuardedJsonBackendApi ServerM
guardedServer token = ( putRecordById token
                   :<|> putRecordFieldById token
                   :<|> putRecordByFile token
                   :<|> answerOPTIONS
                      )

answerOPTIONS = return NoContent

putRecordByFile :: MToken' '["_session"] -> MultipartData Mem -> ServerM [(T.Text,T.Text)]
putRecordByFile token multipartData = do
  runAuth $ guardAuthToken token
  let fInput = fromMaybe "NoPayload" $ fmap fdPayload $ lookupFile "bib" multipartData
  bibRecords <- liftIO $ readRecords fInput
  nTop       <- withDB $ selectList [] [ LimitTo 1
                                       , Desc ReferenceSerial
                                       ]
  let iStart = increaseKey nTop
  res <- zipWithM insertTop [iStart..] bibRecords
  --liftIO $ putStrLn $ show $ map (\(a,b) -> (a, referenceTitle b)) $ lefts res
  let r = map (\(a,b) -> (a, referenceTitle b)) $ lefts res

--  _ <- withDB $ insertMany bibRecords
  -- Tinggal diproses untuk memasukkan fInput ke dalam
  return r

increaseKey :: [Entity Reference] -> Int
increaseKey [] = 1
increaseKey (n:_) =
  let (ReferenceKey k) = entityKey n
   in k + 1

insertTop :: Int -> Reference -> ServerM (Either (T.Text,Reference) Int)
insertTop iInsert rec
  | (T.null $ referenceUrl rec) && (isNothing $ referenceDoi rec) =
              return $ Left ("Empty URL and DOI",rec)
  | otherwise = do
              withDB $ do
                r <- insertUnique $ rec {referenceSerial = iInsert}
                let res = case r of
                        Nothing -> Left ("Identical URL", rec)
                        Just _ -> Right 0
                return res


putRecordById :: MToken' '["_session"] -> Int -> Reference -> ServerM NoContent
putRecordById token serial ref = do
  runAuth $ guardAuthToken token
  liftIO $ putStrLn $ "putRecordById " ++ show serial ++ "/" ++ show ref
  return NoContent

putRecordFieldById :: MToken' '["_session"] -> Int -> T.Text -> T.Text -> ServerM NoContent
putRecordFieldById token serial f c = do
  runAuth $ guardAuthToken token
  liftIO $ putStrLn $ "putRecordFieldById " ++ show serial ++ show f ++ show c
  return NoContent

testEndpoint :: MToken' '["test-permission"] -> ServerM [SimpleRef]
testEndpoint token = do
  runAuth $ guardAuthToken token
  let iPage = 1
  p <- withDB $ selectList [] [ LimitTo resPerPage
                                   , OffsetBy $ (iPage - 1) * resPerPage ]
  liftIO $ putStrLn $ show p
  liftIO $ putStrLn $ "testEndpoint"
  return $ map (fromReference . entityVal) p

staticFiles :: MonadIO m => Tagged m Application
staticFiles = serveDirectoryFileServer "static"

  {-
ssrViews :: Server IsomorphicApi
ssrViews = topView

topView :: Handler (HtmlPage (View Action))
topView = return $ HtmlPage . viewModel . initialModel $ listLink

listView :: Handler (HtmlPage (View Action))
listView = topView
-}

