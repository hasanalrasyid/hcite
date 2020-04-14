module Main where

import           Control.Monad            (replicateM)
import           Data.Maybe               (maybe)
import           Interact
import           System.Environment       (lookupEnv)
import           Text.Read                (read)

main :: IO ()
main = do
  mThreads <- lookupEnv "INTERACT_THREAD_COUNT"
  let threads = maybe 1 read mThreads :: Int
  usersForThreads <- replicateM threads makeRandomUsers
  putStrLn $ "Running nameservice interaction with #threads: " <> show threads
  actionBlock $ head usersForThreads
