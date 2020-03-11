module Monad(
  newServerEnv,
  withDB
  ) where

import Control.Monad.Except
import Database.Persist.Sql

import Config
import qualified Auth.Monad as AM

import qualified Model as M

-- | Create new server environment
newServerEnv :: MonadIO m => ServerConfig -> m AM.ServerEnv
newServerEnv cfg = do
  env@(AM.ServerEnv _ _ pool) <- AM.newServerEnv cfg
  if (initDB cfg) then withDB env $ runMigration M.migrateRefs
                  else return ()
  return env

withDB (AM.ServerEnv _ _ pool) q = do
  liftIO $ flip runSqlPool pool q
