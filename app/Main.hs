module Main where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.STM
import Control.Concurrent.STM.TVar
import Control.Monad (when, unless)
import Control.Concurrent (threadDelay, forkIO, killThread)

import qualified System.Console.Haskeline as HS
import qualified UI.NCurses as NC










main :: IO ()
main = do
  finishVar <- atomically $ newTVar False
  cmdThread <- forkIO $ do
    let loop = do
          mbLine <- HS.getInputLine "> "
          finish <- case mbLine of
            Nothing     -> pure False
            Just "q"    -> liftIO (atomically (writeTVar finishVar True)) >> pure True
            Just _      -> pure False
          unless finish loop
    HS.runInputT HS.defaultSettings loop

  atomically $ do
    finished <- readTVar finishVar
    unless finished retry
  killThread cmdThread

--
-- main :: IO ()
-- main = do
--   NC.runCurses $ do
--     NC.setEcho False
--     w <- NC.defaultWindow
--     NC.updateWindow w $ do
--         NC.moveCursor 1 10
--         NC.drawString "Hello world!"
--         NC.moveCursor 3 10
--         NC.drawString "(press q to quit)"
--         NC.moveCursor 0 0
--     NC.render
--     waitFor w (\ev -> ev == NC.EventCharacter 'q' || ev == NC.EventCharacter 'Q')
--
-- waitFor :: NC.Window -> (NC.Event -> Bool) -> NC.Curses ()
-- waitFor w p = loop where
--     loop = do
--         ev <- NC.getEvent w Nothing
--         case ev of
--             Nothing -> loop
--             Just ev' -> if p ev' then return () else loop
