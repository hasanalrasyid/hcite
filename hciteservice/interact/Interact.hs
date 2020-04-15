module Interact
  ( actionBlock
  , makeRandomUsers
  ) where

import           Control.Monad                     (replicateM, void)
import           Control.Monad.Reader              (ReaderT, runReaderT)
import           Data.Char                         (isHexDigit)
import           Data.Default.Class                (def)
import           Data.Proxy
import           Data.String                       (fromString)
import           Data.String.Conversions           (cs)
import           Data.Text                         (Text,pack)
import qualified Faker.Lorem                       as Lorem
import qualified Faker.Name                        as Name
import qualified Faker.Utils                       as Utils
import           Hciteservice.Application
import qualified Hciteservice.Modules.Hciteservice   as N
import qualified Network.Tendermint.Client         as RPC
import           Servant.API                       ((:<|>) (..))
import           Tendermint.SDK.Application.Module (ApplicationC, ApplicationD,
                                                    ApplicationQ)
import           Tendermint.SDK.BaseApp.Errors     (AppError (..))
import           Tendermint.SDK.BaseApp.Query      (QueryArgs (..),
                                                    QueryResult (..))
import qualified Tendermint.SDK.Modules.Auth       as Auth
import           Tendermint.SDK.Types.Address      (Address (..))
import           Tendermint.Utils.Client           (ClientConfig (..),
                                                    EmptyTxClient (..),
                                                    HasQueryClient (..),
                                                    HasTxClient (..),
                                                    QueryClientResponse (..),
                                                    Signer (..),
                                                    TxClientResponse (..),
                                                    TxOpts (..),
                                                    defaultClientTxOpts)
import           Tendermint.Utils.ClientUtils      (assertTx, rpcConfig)
import           Tendermint.Utils.User             (makeSignerFromUser,
                                                    makeUser)
import           Test.RandomStrings                (onlyWith, randomASCII,
                                                    randomString)
import Data.List (intercalate)
import System.Environment
--------------------------------------------------------------------------------
-- Actions
--------------------------------------------------------------------------------

faucetAccount :: Signer -> Auth.Amount -> IO ()
faucetAccount s@(Signer addr _) amount =
  runAction_ s faucet $ N.FaucetAccountMsg addr N.hciteCoinId amount

createName :: Signer -> Text -> Text -> Text -> IO ()
createName s name val title = buyName s name val title 0

buyName :: Signer -> Text -> Text -> Text -> Auth.Amount -> IO ()
buyName s@(Signer addr _) name newVal title amount =
  runAction_ s buy $ N.BuyNameMsg amount name newVal addr title

deleteName :: Signer -> Text -> IO ()
deleteName s@(Signer addr _) name =
  runAction_ s delete $ N.DeleteNameMsg addr name

setName :: Signer -> Text -> Text -> IO ()
setName s@(Signer addr _) name val =
  runAction_ s set $ N.SetNameMsg name addr val

runAction_
  :: Signer
  -> (TxOpts -> msg -> TxClientM (TxClientResponse () ()))
  -> msg
  -> IO ()
runAction_ s f = void . assertTx . runTxClientM . f (TxOpts 0 s)

actionBlock :: (Signer, Signer) -> IO ()
actionBlock (s1, s2) = do
  (sAddress:sName:_) <- getArgs
  name <- genName
  genCVal <- genWords
  genBVal <- genWords
  genBAmt <- genAmount
  genSVal <- genWords
  putStrLn $ "===============" ++ (intercalate "===" [show name,show genCVal,show genBVal,show genBAmt,show genSVal])
  faucetAccount s2 genBAmt
  putStrLn "2=============="
  createName s1 name genCVal "initiate title"
  putStrLn "3=============="
  buyName s2 name genBVal "this is the title" genBAmt
  putStrLn "4=============="
  setName s2 name genSVal
  putStrLn "5=============="
  theWhois <- retrieveWhois name
  theWhois2 <- retrieveWhois $ pack sName
  putStrLn $ "6=================" ++ show theWhois
  theBalance  <- retrieveBalance "hciteservice"
                  $ fmap N.whoisOwner theWhois
  theBalance2 <- retrieveBalance "hciteservice"
               -- $ Just $ Address "0x46861b9183554254aac5a20ac6498e3b502da90d"
                  $ Just $ Address $ fromString sAddress
  putStrLn $ unlines [ "7================="
                     , show name
                     , show sName
                     , show theWhois
                     , show theWhois2
                     , show theBalance
                     ]
    ++ show theBalance2
  deleteName s2 "quia"

--------------------------------------------------------------------------------
-- Users
--------------------------------------------------------------------------------

makeRandomUsers :: IO (Signer, Signer)
makeRandomUsers = do
  str1 <- randomString (onlyWith isHexDigit randomASCII) 64
  str2 <- randomString (onlyWith isHexDigit randomASCII) 64
  return $ (makeSignerFromUser . makeUser $ str1
           ,makeSignerFromUser . makeUser $ str2
           )

--------------------------------------------------------------------------------
-- Query Client
--------------------------------------------------------------------------------

getAccount
  :: QueryArgs Address
  -> RPC.TendermintM (QueryClientResponse Auth.Account)

getWhois
  :: QueryArgs N.Name
  -> RPC.TendermintM (QueryClientResponse N.Whois)

getBalance
  :: QueryArgs Address
  -> Auth.CoinId
  -> RPC.TendermintM (QueryClientResponse Auth.Coin)

  {-
getWhois     <- Module.Hciteservice.Keeper
getBalance   <- Tendermint.SDK.Modules.Bank.Keeper
getAccount   <- Tendermint.SDK.Modules.Auth.Keeper
  -}
getWhois :<|> getBalance :<|> getAccount =
  genClientQ (Proxy :: Proxy m) queryApiP def
  where
    queryApiP :: Proxy (ApplicationQ HciteserviceModules)
    queryApiP = Proxy

retrieveBalance
  :: Auth.CoinId
  -> Maybe Address
  -> IO (Maybe Auth.Coin)
retrieveBalance _   Nothing = return Nothing
retrieveBalance cId (Just addr) = do
  resp <- queryAction $ getBalance (QueryArgs False addr (-1)) cId
  case resp of
    QueryError e ->
      if appErrorCode e == 2
        then pure Nothing
        else error $ "Unknown nonce error: " <> show (appErrorMessage e)
    QueryResponse QueryResult {queryResultData} ->
      pure $ Just queryResultData


retrieveWhois
  :: Text -> IO (Maybe N.Whois)
retrieveWhois n = do
  resp <- queryAction $ getWhois (QueryArgs False (N.Name n) (-1))
  case resp of
    QueryError e ->
      if appErrorCode e == 2
        then pure Nothing
        else error $ "Unknown nonce error: " <> show (appErrorMessage e)
    QueryResponse QueryResult {queryResultData} ->
      pure $ Just queryResultData

queryAction
  :: RPC.TendermintM a -> IO a
queryAction =
  RPC.runTendermintM rpcConfig

 --------------------------------------------------------------------------------
-- Tx Client
--------------------------------------------------------------------------------

txClientConfig :: ClientConfig
txClientConfig =
  let getNonce addr = do
        resp <- RPC.runTendermintM rpcConfig $ getAccount $
          QueryArgs
            { queryArgsHeight = -1
            , queryArgsProve = False
            , queryArgsData = addr
            }
        -- @NOTE: TxNonce should be +1 of accountNonce
        case resp of
          QueryError e ->
            if appErrorCode e == 2
              then pure 1
              else error $ "Unknown nonce error: " <> show (appErrorMessage e)
          QueryResponse QueryResult {queryResultData} ->
            pure $ 1 + Auth.accountNonce queryResultData

  in ClientConfig
       { clientGetNonce = getNonce
       , clientRPC = rpcConfig
       }

type TxClientM = ReaderT ClientConfig IO

runTxClientM :: TxClientM a -> IO a
runTxClientM m = runReaderT m txClientConfig

-- Hciteservice Client
buy
  :: TxOpts
  -> N.BuyNameMsg
  -> TxClientM (TxClientResponse () ())

set
  :: TxOpts
  -> N.SetNameMsg
  -> TxClientM (TxClientResponse () ())

delete
  :: TxOpts
  -> N.DeleteNameMsg
  -> TxClientM (TxClientResponse () ())

faucet
  :: TxOpts
  -> N.FaucetAccountMsg
  -> TxClientM (TxClientResponse () ())


-- This is for transactions that needs user authorization
-- usually related with CUD of CRUD
-- genClientT for transaction (need authorization/Signer)
-- genClientQ for query (no auth)
(buy :<|> set :<|> delete :<|> faucet) :<|>
  (_ :<|> _) :<|>
  EmptyTxClient =
    genClientT (Proxy @TxClientM) txApiCP txApiDP defaultClientTxOpts
    where
      txApiCP :: Proxy (ApplicationC HciteserviceModules)
      txApiCP = Proxy
      txApiDP :: Proxy (ApplicationD HciteserviceModules)
      txApiDP = Proxy


--------------------------------------------------------------------------------
-- Generation
--------------------------------------------------------------------------------

genWords :: IO Text
genWords = do
  numWords <- Utils.randomNum (1, 10)
  ws <- replicateM numWords Lorem.word
  return . cs . unwords $ ws

genName :: IO Text
genName = fromString <$> Name.name

genAmount :: IO Auth.Amount
genAmount = do
  genAmt <- Utils.randomNum (1, 1000)
  return . fromInteger . toInteger $ genAmt
