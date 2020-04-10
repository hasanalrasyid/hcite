module Monad(
  newServerEnv,
  withDB,
  keyReference,
  fromKeyReference,
  withDBEnv
  ) where

import Control.Monad.Reader
import Database.Persist.Sql

import Config
import qualified Auth.Monad as AM

import qualified Model as M

-- | Create new server environment
newServerEnv :: MonadIO m => ServerConfig -> m AM.ServerEnv
newServerEnv cfg = do
  env <- AM.newServerEnv cfg
  if (initDB cfg) then withDBEnv env $ runMigration M.migrateRefs
                  else return ()
  return env

withDB :: (MonadReader AM.ServerEnv m, MonadIO m)
       => ReaderT SqlBackend IO b -> m b
withDB q = do
  pool <- asks AM.envPool
  liftIO $ runSqlPool q pool

withDBEnv :: MonadIO m
       => AM.ServerEnv -> ReaderT SqlBackend IO a -> m a
withDBEnv (AM.ServerEnv _ _ pool _) q = do
  liftIO $ flip runSqlPool pool q

keyReference :: Int -> Key M.Reference
keyReference x = M.ReferenceKey x

fromKeyReference :: Key M.Reference -> Int
fromKeyReference (M.ReferenceKey x) = x
