module Monad(
  newServerEnv,
  withDB,
  withDBEnv
  ) where

import Control.Monad.Reader
import Control.Monad.Except
import Database.Persist.Sql

import Config
import qualified Auth.Monad as AM

import qualified Model as M

-- | Create new server environment
newServerEnv :: MonadIO m => ServerConfig -> m AM.ServerEnv
newServerEnv cfg = do
  env@(AM.ServerEnv _ _ pool) <- AM.newServerEnv cfg
  if (initDB cfg) then withDBEnv env $ runMigration M.migrateRefs
                  else return ()
  return env

withDB q = do
  pool <- asks AM.envPool
  liftIO $ runSqlPool q pool

withDBEnv (AM.ServerEnv _ _ pool) q = do
  liftIO $ flip runSqlPool pool q
