module Main where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.STM
import Control.Concurrent.STM.TVar
import Control.Monad (when, unless)
import Control.Concurrent (threadDelay, forkIO)

import qualified System.Console.Haskeline as HS


main :: IO ()
main = do
  finishVar <- atomically $ newTVar False

  cmdThread <- forkIO $ do
    let loop = do
          mbLine <- HS.getInputLine "> "
          finish <- case mbLine of
            Nothing     -> pure False
            Just "quit" -> liftIO (atomically (writeTVar finishVar True)) >> pure True
            Just _      -> pure False
          unless finish loop
    HS.runInputT HS.defaultSettings loop

  atomically $ do
    finished <- readTVar finishVar
    unless finished retry
  threadDelay 1000000
