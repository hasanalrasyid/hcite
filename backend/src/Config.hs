module Config(
    ServerConfig(..)
  , readConfig
  , createPool
  ) where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Logger
import Data.Aeson
import Data.Monoid
import Data.Text
import Data.Text.Encoding
import Data.Yaml.Config

import Database.Persist.MySQL

-- | Startup configuration of server
data ServerConfig = ServerConfig {
  -- | Server host name
  serverHost                 :: !Text
  -- | Server port number
, serverPort                 :: !Int
  -- | DB host
, serverDBHost               :: !Text
  -- | DB port
, serverDBPort               :: !Int
  -- | DB user
, serverDBUser               :: !Text
  -- | DB user password
, serverDBPass               :: !Text
  -- | DB database
, serverDBBase               :: !Text
  -- | DB pool size
, serverDBSize               :: !Int
, initDB                     :: !Bool
}

instance FromJSON ServerConfig where
  parseJSON (Object o) = ServerConfig
    <$> o .: "host"
    <*> o .: "port"
    <*> o .: "db-host"
    <*> o .: "db-port"
    <*> o .: "db-user"
    <*> o .: "db-pass"
    <*> o .: "db-base"
    <*> o .: "db-size"
    <*> o .: "init-db"
  parseJSON _ = mzero

readConfig :: MonadIO m => FilePath -> m ServerConfig
readConfig f = liftIO $ loadYamlSettings [f] [] useEnv

connectionInfo = setMySQLConnectInfoCharset 45 $ mkMySQLConnectInfo "localhost" "test" "kadal123@55"     "example"

-- | Create connection pool to postgres
createPool :: ServerConfig -> IO ConnectionPool
--createPool ServerConfig{..} = runStdoutLoggingT $ createPostgresqlPool constr serverDBSize
createPool ServerConfig{..} = runStdoutLoggingT $ createMySQLPool connectionInfo serverDBSize
  where
    constr = encodeUtf8 $ "host=" <> serverDBHost <> " port=" <> (pack . show $ serverDBPort)
      <> " user=" <> serverDBUser <> " password=" <> serverDBPass <> " database=" <> serverDBBase