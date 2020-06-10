module Main where

import qualified Data.Map as Map
import qualified Data.Set as Set

import Data.IORef (newIORef, modifyIORef, writeIORef, readIORef)

import Control.Monad.IO.Class (liftIO)
import Control.Monad.STM
import Control.Concurrent.STM.TVar
import Control.Monad (when, unless, forever)
import Control.Concurrent (ThreadId, threadDelay, forkIO, killThread)

import qualified System.Console.Haskeline as HS
import qualified UI.NCurses as NC

import Morse

{-

ticker -> signal (10 sec)

on signal -> char signals (0.5 sec) "Hello, world!"

on char signal ->
  uppercase
  filter letters
  toMorse
  (groupBy 5)
  print
-}

startTicker :: TVar Bool -> Int -> IO ThreadId
startTicker ticksVar ms = do
  let loop = do
        threadDelay ms
        atomically $ writeTVar ticksVar True

        atomically $ do
          processed <- readTVar ticksVar
          unless processed retry

  forkIO $ forever loop

startTickerIdx :: TVar Bool -> TVar (Int, Int) -> IO ThreadId
startTickerIdx ticksVar ticksIdxVar = do
  let loop = do
        atomically $ do
          tick <- readTVar ticksVar
          unless tick retry

        atomically $ do
          (idx, total) <- readTVar ticksIdxVar
          let idx' | idx + 1 == total = 0
                   | otherwise = idx + 1
          writeTVar ticksIdxVar (idx', total)

        atomically $ writeTVar ticksVar False
  forkIO $ forever loop


startCharProducer :: TVar Bool -> Int -> String -> IO (TVar (Maybe Char, Bool))
startCharProducer _ _ [] = error "empty string"
startCharProducer ticksVar ms str = do

    atomically $ do
      tick <- readTVar ticksVar
      unless tick retry

    charSignalVar <- newTVarIO (Nothing, False)
    let loop chs = case chs of
          [] -> pure ()
          (c:cs) -> do
            atomically $ writeTVar charSignalVar $ (Just c, True)
            threadDelay ms
            loop cs

    forkIO $ do
      loop str
      atomically $ writeTVar charSignalVar (Nothing, True)

    pure charSignalVar

startStrBuffer :: TVar (Maybe Char, Bool) -> IO (TVar String)
startStrBuffer charSignalVar = do
  strBufferVar <- newTVarIO ""
  forkIO $ forever $ atomically $ do
    (mbCh, changed) <- readTVar charSignalVar
    unless changed retry
    writeTVar charSignalVar (mbCh, False)
    case mbCh of
      Nothing -> writeTVar strBufferVar ""
      Just ch -> modifyTVar strBufferVar (<> [ch])
  pure strBufferVar

main :: IO ()
main = do
  ticksVar    <- atomically $ newTVar False
  ticksIdxVar <- atomically $ newTVar (0, 10)

  _   <- startTicker ticksVar $ 1000 * 1000 * 2
  charSignalVar <- startCharProducer ticksVar (1000 * 500) "Hello, world!"
  strBufferVar <- startStrBuffer charSignalVar
  _ <- startTickerIdx ticksVar ticksIdxVar

  NC.runCurses $ do
    NC.setEcho False
    w <- NC.defaultWindow
    NC.updateWindow w $ do
        NC.moveCursor 1 10
        NC.drawString "(press q to quit)"
        NC.moveCursor 0 0
    NC.render
    doLoop w ticksIdxVar charSignalVar strBufferVar

doLoop
  :: NC.Window
  -> TVar (Int, Int)
  -> TVar (Maybe Char, Bool)
  -> TVar String
  -> NC.Curses ()
doLoop w ticksIdxVar charSignalVar strBufferVar = do
  loop
  where
    renderData = do
      (idx, total) <- liftIO $ readTVarIO ticksIdxVar
      (mbCh, _)    <- liftIO $ readTVarIO charSignalVar
      strBuf       <- liftIO $ readTVarIO strBufferVar
      let rendered = replicate idx '-' <> "#" <> replicate (total - idx - 1) '-'
      let idxRendered = show (idx, total)
      let renderedCh = case mbCh of
            Nothing -> " "
            Just ch -> [ch]

      NC.updateWindow w $ do
          NC.moveCursor 3 10
          NC.drawString rendered

          NC.moveCursor 3 30
          NC.drawString idxRendered

          NC.moveCursor 4 10
          NC.drawString renderedCh

          NC.moveCursor 5 10
          NC.drawString strBuf

          NC.moveCursor 0 0
      NC.render

    loop = do
      ev <- NC.getEvent w (Just 0)
      case ev of
        Just (NC.EventCharacter 'q') -> pure ()
        Just (NC.EventCharacter 'Q') -> pure ()
        _ -> renderData >> loop
