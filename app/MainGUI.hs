{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Concurrent
import Control.Concurrent.MVar
import Control.Monad
import Control.Monad.State
import Control.Monad.Trans
import qualified Data.List.Zipper as LZ
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

drawRun :: Int -> Msec -> RunDef -> [Run] -> Run -> CairoWidget (V Dim) (V Dim) (StyleT IO)
drawRun cur t def runs run = alignLeft (fixh 100 (fixw 500 (scale drawTime))) `topOf` drawSplits
  where
    drawTime :: CairoWidget (F Dim) (F Dim) (StyleT IO)
    drawTime = text (formatMsec t)
    drawSplits :: CairoWidget (V Dim) (V Dim) (StyleT IO)
    drawSplits = listOf (fmap (flip showSplit) splitZipper)
    splitZipper = LZ.Zip (reverse [0..(cur-1)]) [cur..(length (rdSplits def) - 1)]
    showSplit :: Bool -> Int -> CairoWidget (V Dim) (F Dim) (StyleT IO)
    showSplit True i = box (showSplit' i)
    showSplit False i = showSplit' i
    showSplit' :: Int -> CairoWidget (V Dim) (F Dim) (StyleT IO)
    showSplit' i
      | i == length (rSplitTimes run) =
          let diffToBest = (t -) <$> bestSums def runs !! i
              lastDiffToBest = if i > 0 then (-) <$> (rSplitTimes run !! (i-1)) <*> (bestSums def runs !! (i-1)) else InvalidSplit
          in tabularH
            [ (0.5, alignLeft (text (unpack $ rdSplits def !! i)))
            , (0.5, drawSplitTime (ValidSplit t) diffToBest lastDiffToBest)
            ]
      | i < length (rSplitTimes run) =
          let diffToBest = (-) <$> (rSplitTimes run !! i) <*> bestSums def runs !! i
              lastDiffToBest = if i > 0 then (-) <$> (rSplitTimes run !! (i-1)) <*> (bestSums def runs !! (i-1)) else InvalidSplit
          in tabularH
            [ (0.5, alignLeft (text (unpack $ rdSplits def !! i)))
            , (0.5, drawSplitTime (rSplitTimes run !! i) diffToBest lastDiffToBest)
            ]
      | i > length (rSplitTimes run) =
          tabularH
            [ (1.0, alignLeft (text (unpack $ rdSplits def !! i)))
            ]

drawSplit :: Split (CairoWidget (V Dim) (F Dim) (StyleT IO)) -> CairoWidget (V Dim) (F Dim) (StyleT IO)
drawSplit InvalidSplit = alignLeft (text "---")
drawSplit (ValidSplit w) = w

drawSplitTime :: Split Msec -> Split Msec -> Split Msec -> CairoWidget (V Dim) (F Dim) (StyleT IO)
drawSplitTime t diffToBest lastDiffToBest =
  let color | fromSplit 0 diffToBest <= 0 = RGB 0 1 0
            | fromSplit 0 lastDiffToBest > fromSplit 0 diffToBest = RGB 1 1 0
            | otherwise = RGB 1 0 0
  in withStyling (color1 color) $ tabularH
    [ (0.5, alignLeft (text (formatSplit t)))
    , (0.5, alignLeft (text (fromSplit "" (formatMsecShort <$> diffToBest))))
    ]

main :: IO ()
main = batchMain $ \draw' evchan -> do
  f <- loadFont "Droid Sans 12"
  let draw = draw' . withStyling (font f)
  runDef <- loadRunDef "./def"
  runs <- loadRuns "./runs"

  gh <- GH.initGlobalHotkeys

  sem <- newEmptyMVar
  GH.setGlobalHotkey gh GH.xK_space GH.noModMask (putMVar sem True)
  GH.setGlobalHotkey gh GH.xK_Escape GH.noModMask (putMVar sem False)

  date <- getPOSIXTime
  (t, run) <- flip runStateT (emptyRun date) $ do
    beforeStart <- get
    liftIO $ draw $ drawRun 0 0 runDef runs beforeStart

    liftIO $ takeMVar sem
    start <- liftIO $ posixToMsec <$> getPOSIXTime

    forM_ [0..(length (rdSplits runDef) - 1)] $ \i -> do
      run <- get

      let best = bestSplits runDef runs !! i
          bestSum = bestSums runDef runs !! i
          lastSplit = case rSplitTimes run of
                        [] -> ValidSplit 0
                        xs -> last xs

      r <- liftIO $ loopMVar sem $ do
        t <- (subtract start) <$> posixToMsec <$> getPOSIXTime
        draw $ drawRun i t runDef runs run

      split <- liftIO $ if r then ValidSplit <$> subtract start <$> posixToMsec <$> getPOSIXTime else return InvalidSplit
      modify $ \run -> run { rSplitTimes = rSplitTimes run ++ [split] }

    end <- liftIO $ posixToMsec <$> getPOSIXTime
    return (end - start)

  draw $ drawRun (length (rdSplits runDef)) t runDef runs run `topOf` alignLeft (text "Space to save, escape to discard.")

  save <- takeMVar sem

  if save then writeRuns "./runs" (runs ++ [run]) else return ()
