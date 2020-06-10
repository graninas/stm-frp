module Main where

import qualified Data.Map as Map
import qualified Data.Set as Set

import Control.Monad.IO.Class (liftIO)
import Control.Monad.STM
import Control.Concurrent.STM.TVar
import Control.Monad (when, unless, forever)
import Control.Concurrent (threadDelay, forkIO, killThread)

import qualified System.Console.Haskeline as HS
import qualified UI.NCurses as NC

import Morse

-- TODO: micros should be a variable
startTicker micros = do
  ticksVar <- atomically $ newTVar False

  threadId <- forkIO $ forever $ do
    threadDelay micros
    atomically $ writeTVar ticksVar True
    -- Should be a lag here? Will the threads resume anyway?
    -- threadDelay 100
    -- atomically $ writeTVar ticksVar False
    atomically $ do
      tickProcessed <- readTVar ticksVar
      unless tickProcessed retry

  pure (ticksVar, threadId)


-- main :: IO ()
-- main = do
--   finishVar <- atomically $ newTVar False
--
--   (ticksVar, tickerThreadId) <- startTicker 100
--
--   cmdThreadId <- forkIO $ do
--     let loop = do
--           mbLine <- HS.getInputLine "> "
--           finish <- case mbLine of
--             Nothing     -> pure False
--             Just "q"    -> liftIO (atomically (writeTVar finishVar True)) >> pure True
--             Just _      -> pure False
--           unless finish loop
--     HS.runInputT HS.defaultSettings loop
--
--   atomically $ do
--     finished <- readTVar finishVar
--     unless finished retry
--
--   killThread cmdThreadId
--   killThread tickerThreadId


startTickerIdx ticksVar ticksIdxVar = forkIO $ forever $ do

  atomically $ do
    tick         <- readTVar ticksVar
    (idx, total) <- readTVar ticksIdxVar
    unless tick retry
    let idx' | idx + 1 == total = 0
             | otherwise = idx + 1
    writeTVar ticksIdxVar (idx', total)
    writeTVar ticksVar False

main :: IO ()
main = do
  ticksVar    <- atomically $ newTVar False
  ticksIdxVar <- atomically $ newTVar (0, 10)

  (ticksVar, tickerThreadId) <- startTicker $ 2 * 1000 * 1000

  tickerIdxThreadId <- startTickerIdx ticksVar ticksIdxVar

  NC.runCurses $ do
    NC.setEcho False
    w <- NC.defaultWindow
    NC.updateWindow w $ do
        NC.moveCursor 1 10
        NC.drawString "(press q to quit)"
        NC.moveCursor 0 0
    NC.render
    doLoop w ticksIdxVar

doLoop :: NC.Window -> TVar (Int, Int) -> NC.Curses ()
doLoop w ticksIdxVar = loop
  where
    renderData = do
      (idx, total) <- liftIO $ readTVarIO ticksIdxVar
      let rendered = replicate idx '-' <> "#" <> replicate (total - idx - 1) '-'
      let idxRendered = show (idx, total)
      NC.updateWindow w $ do
          NC.moveCursor 3 10
          NC.drawString rendered

          NC.moveCursor 3 30
          NC.drawString idxRendered

          NC.moveCursor 0 0
      NC.render

    loop = do
      ev <- NC.getEvent w (Just 0)
      -- ev <- NC.getEvent w Nothing

      case ev of
        Just (NC.EventCharacter 'q') -> pure ()
        Just (NC.EventCharacter 'Q') -> pure ()
        _ -> renderData >> loop
