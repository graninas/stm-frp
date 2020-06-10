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

main :: IO ()
main = do
  ticksVar    <- atomically $ newTVar False
  ticksIdxVar <- atomically $ newTVar (0, 10)

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
      let rendered = ""
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
      case ev of
        Just (NC.EventCharacter 'q') -> pure ()
        Just (NC.EventCharacter 'Q') -> pure ()
        _ -> renderData >> loop
