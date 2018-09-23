module GlobalHotkeys
  ( GlobalHotkeys
  , setGlobalHotkey
  , initGlobalHotkeys
  , module Graphics.X11.Types
  ) where

import Control.Concurrent
import Control.Monad
import Data.IORef
import qualified Data.Map as Map
import Graphics.X11.Xlib
import Graphics.X11.Xlib.Extras
import Graphics.X11.Types

data GlobalHotkeys = GlobalHotkeys
  { ghDisplay :: Display
  , ghWindow :: Window
  , ghMap :: IORef (Map.Map (KeyCode, KeyMask) (IO ()))
  }

initGlobalHotkeys :: IO GlobalHotkeys
initGlobalHotkeys = do
  display <- openDisplay ""
  let window = defaultRootWindow display
  -- window <- createSimpleWindow display root 0 0 10 10 0 0 0
  mapRef <- newIORef Map.empty
  selectInput display window keyPressMask

  forkIO $ allocaXEvent $ \xev -> forever $ do
    nextEvent display xev
    (_, _, _, _, _, _, _, mask, code, _) <- get_KeyEvent xev
    m <- readIORef mapRef
    case Map.lookup (code, mask) m of
      Nothing -> return ()
      Just action -> do
        ungrabKeyboard display currentTime
        action

  return GlobalHotkeys
    { ghDisplay = display
    , ghWindow = window
    , ghMap = mapRef
    }

setGlobalHotkey :: GlobalHotkeys -> KeySym -> KeyMask -> IO () -> IO ()
setGlobalHotkey gh sym mask action = do
  code <- keysymToKeycode (ghDisplay gh) sym
  modifyIORef (ghMap gh) $ Map.insert (code, mask) action
  grabKey (ghDisplay gh) code mask (ghWindow gh) True grabModeSync grabModeSync
