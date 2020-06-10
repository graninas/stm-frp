module Prototype where


import qualified Data.Map as Map
import qualified Data.Set as Set

import Control.Monad.IO.Class (liftIO)
import Control.Monad.STM
import Control.Concurrent.STM.TVar
import Control.Monad (when, unless, forever)
import Control.Concurrent (threadDelay, forkIO, killThread)


startTicker micros = do
  ticksVar <- atomically $ newTVar False

  threadId <- forkIO $ forever $ do
    threadDelay micros
    atomically $ writeTVar ticksVar True
    atomically $ do
      tickProcessed <- readTVar ticksVar
      unless tickProcessed retry

  pure (ticksVar, threadId)

startTickerIdx ticksVar ticksIdxVar = forkIO $ forever $ do

  atomically $ do
    tick         <- readTVar ticksVar
    (idx, total) <- readTVar ticksIdxVar
    unless tick retry
    let idx' | idx + 1 == total = 0
             | otherwise = idx + 1
    writeTVar ticksIdxVar (idx', total)
    writeTVar ticksVar False
