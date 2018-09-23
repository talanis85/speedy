{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Concurrent
import Control.Concurrent.MVar
import Control.Monad
import Control.Monad.State
import Control.Monad.Trans
import Data.Text.Lazy (Text)
import Data.Time.Clock
import Data.Time.Clock.POSIX
import Text.Printf
import System.Directory
import System.IO

import qualified GlobalHotkeys as GH
import Types

loopMVar :: MVar a -> (IO ()) -> IO a
loopMVar mvar action = do
  action
  ret <- tryTakeMVar mvar
  case ret of
    Nothing -> threadDelay 100000 >> loopMVar mvar action
    Just r -> return r

main :: IO ()
main = do
  runDef <- loadRunDef "./def"
  runs <- loadRuns "./runs"

  gh <- GH.initGlobalHotkeys

  sem <- newEmptyMVar
  GH.setGlobalHotkey gh GH.xK_space GH.noModMask (putMVar sem True)
  GH.setGlobalHotkey gh GH.xK_Escape GH.noModMask (putMVar sem False)

  printf "  RUN: %s    GOAL: %s\n\n" (rdTitle runDef) (rdGoal runDef)
  printf "Press space to start!\n\n"

  takeMVar sem

  date <- getPOSIXTime

  printf "%20s   %10s   %10s   %10s   %10s   %10s\n"
    ("SPLIT" :: Text)
    ("SUM TIME" :: Text)
    ("SPLIT TIME" :: Text)
    ("BEST SUM" :: Text)
    ("BEST SPLIT" :: Text)
    ("DIFF" :: Text)
  putStrLn (replicate 80 '-')

  start <- posixToMsec <$> getPOSIXTime

  run <- flip execStateT (emptyRun date) $
    forM_ [0..(length (rdSplits runDef) - 1)] $ \i -> do
      run <- get

      let best = bestSplits runDef runs !! i
          bestSum = bestSums runDef runs !! i
          lastSplit = case rSplitTimes run of
                        [] -> ValidSplit 0
                        xs -> last xs

      r <- liftIO $ loopMVar sem $ do
        t <- (subtract start) <$> posixToMsec <$> getPOSIXTime
        printf "\r%20s   %10s   %10s   %10s   %10s   %10s"
          (rdSplits runDef !! i)
          (formatMsec t)
          (formatSplit (fmap (t -) lastSplit))
          (formatSplit bestSum)
          (formatSplit best)
          (formatSplit (fmap (t -) bestSum))
        hFlush stdout

      split <- liftIO $ if r then ValidSplit <$> subtract start <$> posixToMsec <$> getPOSIXTime else return InvalidSplit
      modify $ \run -> run { rSplitTimes = rSplitTimes run ++ [split] }
      liftIO $ putStrLn ""

  print run

  putStrLn "Save this run? (y/n) "
  hSetBuffering stdin NoBuffering
  c <- getChar

  if c == 'y' then writeRuns "./runs" (runs ++ [run]) else return ()
