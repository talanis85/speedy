{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Concurrent
import Control.Concurrent.MVar
import Control.Monad
import Control.Monad.State
import Control.Monad.Trans
import qualified Data.List.Zipper as LZ
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Text.Lazy (Text, unpack)
import Data.Time.Clock
import Data.Time.Clock.POSIX
import Text.Printf
import System.Directory
import System.IO

import Graphics.Disguise.Cairo
import Graphics.Disguise.Gtk.Event
import Graphics.Disguise.Gtk.Main

import qualified GlobalHotkeys as GH
import Types

loopMVar :: MVar a -> (IO ()) -> IO a
loopMVar mvar action = do
  action
  ret <- tryTakeMVar mvar
  case ret of
    Nothing -> threadDelay 100000 >> loopMVar mvar action
    Just r -> return r

drawRun :: Either Bool Text -> Msec -> RunDef -> [RunInfo] -> RunInfo -> CairoWidget (V Dim) (V Dim) (StyleT IO)
drawRun cur t def runInfos run = alignLeft (fixh 100 (fixw 500 (scale mainTime))) `topOf` runList
  where
    mainTime :: CairoWidget (F Dim) (F Dim) (StyleT IO)
    mainTime = text (formatMsec t)

    runList :: CairoWidget (V Dim) (V Dim) (StyleT IO)
    runList = listOf splitZipper

    (doneSplits, currentSplits, todoSplits) = case cur of
      Left False -> ([], [], rdSplits def)
      Left True -> (rdSplits def, [], [])
      Right cur' -> let (a, b) = break (== cur') (rdSplits def) in (a, [head b], tail b)

    splitZipper = LZ.Zip (map (withSelectionBox drawDoneSplit) (reverse doneSplits))
                         (map (withSelectionBox drawCurrentSplit) currentSplits
                          ++ map (withSelectionBox drawTodoSplit) todoSplits)

    withSelectionBox f name True = box (f name)
    withSelectionBox f name False = f name

    runs = map rRun runInfos

    drawTodoSplit name = tabularH
      [ (0.5, alignLeft (text (unpack name)))
      , (0.5, alignLeft (drawTime (drawTodoTime <$> unAbsolute <$> lookupTime name (best runs))))
      ]

    drawDoneSplit name = tabularH
      [ (0.5, alignLeft (text (unpack name)))
      , (0.2, alignLeft (drawTime (drawDoneTime <$> unAbsolute <$> lookupTime name (rRun run))))
      , let diff = subtract <$> (unAbsolute <$> lookupTime name (best runs)) <*> (unAbsolute <$> lookupTime name (rRun run))
        in (0.3, alignLeft (drawTime (drawDoneTime <$> diff)))
      ]

    drawCurrentSplit name = tabularH
      [ (0.5, alignLeft (text (unpack name)))
      , (0.2, alignLeft (text (formatMsecShort t)))
      , let diff = subtract <$> (unAbsolute <$> lookupTime name (best runs)) <*> pure t
        in (0.3, alignLeft (drawTime (drawDoneTime <$> diff)))
      ]

    drawTime InvalidTime = text "---"
    drawTime (ValidTime w) = w

    drawDoneTime = text . formatMsecShort
    drawTodoTime = text . formatMsecShort

main :: IO ()
main = batchMain $ \draw' evchan -> do
  f <- loadFont "Droid Sans 12"
  let draw = draw' . withStyling (font f)
  runDef <- loadRunDef "./def"
  runs <- loadRunInfos "./runs"

  gh <- GH.initGlobalHotkeys

  sem <- newEmptyMVar
  GH.setGlobalHotkey gh GH.xK_space GH.noModMask (putMVar sem True)
  GH.setGlobalHotkey gh GH.xK_Escape GH.noModMask (putMVar sem False)

  date <- getPOSIXTime
  (t, run) <- flip runStateT (initRunInfo date) $ do
    beforeStart <- get
    liftIO $ draw $ drawRun (Left False) 0 runDef runs beforeStart

    liftIO $ takeMVar sem
    start <- liftIO $ posixToMsec <$> getPOSIXTime

    forM_ (rdSplits runDef) $ \name -> do
      run <- get

      r <- liftIO $ loopMVar sem $ do
        t <- (subtract start) <$> posixToMsec <$> getPOSIXTime
        draw $ drawRun (Right name) t runDef runs run

      split <- liftIO $ if r then ValidTime <$> Absolute <$> subtract start <$> posixToMsec <$> getPOSIXTime else return InvalidTime
      modify $ \run -> run { rRun = Map.insert name split (rRun run) }

    end <- liftIO $ posixToMsec <$> getPOSIXTime
    return (end - start)

  draw $ drawRun (Left True) t runDef runs run `topOf` alignLeft (text "Space to save, escape to discard.")

  save <- takeMVar sem

  if save then writeRunInfos "./runs" (runs ++ [run]) else return ()
