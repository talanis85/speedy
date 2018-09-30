{-# LANGUAGE OverloadedStrings #-}
module Plotter.GnuPlot where

import Control.Monad.Writer
import Data.List
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Maybe
import Data.Text.Lazy (Text, pack)
import Data.Time.Clock
import Data.Time.Clock.POSIX
import Data.Time.Format
import Text.Printf

import Types

plotRuns :: RunDef -> [RunInfo] -> Text
plotRuns runDef runInfos = execWriter $ do
  let maxTime = maximum $ mapMaybe maybeTime $ concat $ map (Map.elems . rRun) runInfos

  tell "set ydata time\n"
  tell "set timefmt \"%H:%M:%S\"\n"
  tell $ pack $ printf "set yrange [0 : \"%s\"]\n" (formatMsec maxTime)

  forM_ (zip [1..] runInfos) $ \(j, runInfo) -> do
    tell $ pack $ printf "$d%d << EOD\n" (j :: Int)
    tell "  0 \"Start\" 00:00:00.0\n"
    forM_ (zip [1..] (rdSplits runDef)) $ \(i, split) -> do
      case lookupTime split (rRun runInfo) of
        Just t -> tell $ pack $ printf "  %d \"%s\" %s\n" (i :: Int) split (formatMsec t)
        Nothing -> return ()
    tell "EOD\n"

  tell "plot "

  let strs = flip map (zip [1..] runInfos) $ \(j, runInfo) ->
        let runDate = formatTime defaultTimeLocale "%c" (posixSecondsToUTCTime (msecToPosix (rDate runInfo)))
        in printf "'$d%d' using 1:3:xtic(2) title \"%s\" with lines" (j :: Int) runDate
  tell $ pack $ printf (intercalate "," strs)
  tell "\n"

plotSums :: RunDef -> [RunInfo] -> Text
plotSums runDef runInfos = execWriter $ do
  let minDate = minimum $ map rDate runInfos
      maxDate = maximum $ map rDate runInfos

  tell "set timefmt \"%Y-%m-%d-%H:%M:%S\"\n"
  tell "set xdata time\n"
  tell $ pack $ printf "set xrange [\"%s\" : \"%s\"]\n"
    (formatTime defaultTimeLocale "%Y-%m-%d-%H:%M:%S" (posixSecondsToUTCTime (msecToPosix minDate)))
    (formatTime defaultTimeLocale "%Y-%m-%d-%H:%M:%S" (posixSecondsToUTCTime (msecToPosix maxDate)))

  let maxTime = maximum $ mapMaybe maybeTime $ concat $ map (Map.elems . rRun) runInfos
  tell "set ydata time\n"
  tell $ pack $ printf "set yrange [\"1970-01-01-00:00:00.0\" : \"1970-01-01-%s\"]\n" (formatMsec maxTime)

  tell "$data << EOD\n"
  forM_ (zip [1..] runInfos) $ \(j, runInfo) -> do
    tell $ pack $ printf "%s 1970-01-01-%s\n"
      (formatTime defaultTimeLocale "%Y-%m-%d-%H:%M:%S" (posixSecondsToUTCTime (msecToPosix (rDate runInfo))))
      (formatMsec (maximum (mapMaybe maybeTime (Map.elems (rRun runInfo)))))
  tell "EOD\n"
  tell $ pack $ printf "plot '$data' using 1:2 title \"%s - %s\" with lines\n" (rdTitle runDef) (rdGoal runDef)
