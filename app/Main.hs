{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Main where

import Control.Concurrent
import Control.Concurrent.MVar
import Control.Monad
import Control.Monad.State
import Control.Monad.Trans
import Data.List
import qualified Data.List.Zipper as LZ
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Maybe
import Data.Monoid
import Data.Text.Lazy (Text, unpack)
import Data.Time.Clock
import Data.Time.Clock.POSIX
import Data.Time.Format
import Development.GitRev
import Options.Applicative
import Text.Printf
import System.Directory
import System.IO

import Graphics.Disguise.Cairo
import Graphics.Disguise.Gtk.Event
import Graphics.Disguise.Gtk.Main

import qualified GlobalHotkeys as GH
import Types

data Options = Options
  { optCommand :: IO ()
  }

version :: String
version = $(gitBranch) ++ "@" ++ $(gitHash)

commandLineP :: ParserInfo Options
commandLineP = info (helper <*> (Options <$> commandP))
  (  fullDesc
  <> progDesc "A speedrun timer"
  <> header "speedy - A speedrun timer"
  <> footer ("Version: " ++ version)
  )

commandP :: Parser (IO ())
commandP = hsubparser
  (  command "list"     (info (pure cmdList)
                              (progDesc "List all runs."))
  <> command "show"     (info (cmdShow <$> runNumberArgument)
                              (progDesc "Show a specific run."))
  <> command "plot"     (info plotP
                              (progDesc "Output various statistics as a GNUplot script"))
  ) <|> pure cmdRun

plotP :: Parser (IO ())
plotP = hsubparser
  (  command "runs"     (info (pure cmdPlotRuns)
                              (progDesc "Overlay all runs on top of each other"))
  <> command "sums"     (info (pure cmdPlotSums)
                              (progDesc "Plot the sum times in chronological order"))
  )

runNumberArgument = argument auto (metavar "RUN")

main :: IO ()
main = do
  opts <- execParser commandLineP
  optCommand opts

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
      , (0.5, alignLeft (drawTime (text <$> formatMsec <$> unAbsolute <$> lookupTime name (best runs))))
      ]

    drawDoneSplit name = tabularH
      [ (0.5, alignLeft (text (unpack name)))
      , (0.2, alignLeft (drawTime (text <$> formatMsec <$> unAbsolute <$> lookupTime name (rRun run))))
      , let diff = subtract <$> (unAbsolute <$> lookupTime name (best runs)) <*> (unAbsolute <$> lookupTime name (rRun run))
        in (0.3, alignLeft (drawTime (text <$> formatMsecShort <$> diff)))
      ]

    drawCurrentSplit name = tabularH
      [ (0.5, alignLeft (text (unpack name)))
      , (0.2, alignLeft (text (formatMsec t)))
      , let diff = subtract <$> (unAbsolute <$> lookupTime name (best runs)) <*> pure t
        in (0.3, alignLeft (drawTime (text <$> formatMsecShort <$> diff)))
      ]

    drawTime InvalidTime = text "---"
    drawTime (ValidTime w) = w

    isBetter time bestTime = fromTime False ((<) <$> time <*> bestTime)
    splitColor absTime bestAbsTime relTime bestRelTime
      | isBetter relTime bestRelTime = RGB 0.5 1 1
      | isBetter absTime bestAbsTime = RGB 0.5 1 0.5
      | otherwise = RGB 1 0.5 0.5

cmdRun :: IO ()
cmdRun = batchMain $ \draw' evchan -> do
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

cmdList :: IO ()
cmdList = do
  runDef <- loadRunDef "./def"
  runs <- loadRunInfos "./runs"

  forM_ (zip [0..] runs) $ \(i, runInfo) -> do
    printf "%-5d %s\n" (i :: Int) (formatTime defaultTimeLocale "%c" (posixSecondsToUTCTime (msecToPosix (rDate runInfo))))

cmdShow :: Int -> IO ()
cmdShow n = do
  runDef <- loadRunDef "./def"
  runs <- loadRunInfos "./runs"

  let absoluteRun = rRun $ runs !! n
  let relativeRun = toRelative (rdSplits runDef) absoluteRun
  let bestRelative = best $ map (toRelative (rdSplits runDef) . rRun) runs

  forM_ (rdSplits runDef) $ \name -> do
    let absoluteTime = unAbsolute <$> lookupTime name absoluteRun
    let relativeTime = unRelative <$> lookupTime name relativeRun
    let absoluteDiff = subtract <$> (unAbsolute <$> lookupTime name (best (rRun <$> runs))) <*> absoluteTime
    let relativeDiff = subtract <$> (unRelative <$> lookupTime name bestRelative) <*> relativeTime
    printf "%30s %10s %10s %10s %10s\n"
      name
      (fromTime "---" (fmap formatMsec absoluteTime))
      (fromTime "---" (fmap formatMsec relativeTime))
      (fromTime "---" (fmap formatMsecShort absoluteDiff))
      (fromTime "---" (fmap formatMsecShort relativeDiff))

cmdPlotRuns :: IO ()
cmdPlotRuns = do
  runDef <- loadRunDef "./def"
  runInfos <- loadRunInfos "./runs"

  let maxTime = unAbsolute $ maximum $ mapMaybe maybeTime $ concat $ map (Map.elems . rRun) runInfos

  putStrLn "set ydata time"
  putStrLn "set timefmt \"%H:%M:%S\""
  printf "set yrange [0 : \"%s\"]\n" (formatMsec maxTime)

  forM_ (zip [1..] runInfos) $ \(j, runInfo) -> do
    printf "$d%d << EOD\n" (j :: Int)
    printf "  0 \"Start\" 00:00:00.0\n"
    forM_ (zip [1..] (rdSplits runDef)) $ \(i, split) -> do
      case lookupTime split (rRun runInfo) of
        ValidTime t -> printf "  %d \"%s\" %s\n" (i :: Int) split (formatMsec (unAbsolute t))
        InvalidTime -> return ()
    printf "EOD\n"

  printf "plot "

  let strs = flip map (zip [1..] runInfos) $ \(j, runInfo) ->
        let runDate = formatTime defaultTimeLocale "%c" (posixSecondsToUTCTime (msecToPosix (rDate runInfo)))
        in printf "'$d%d' using 1:3:xtic(2) title \"%s\" with lines" (j :: Int) runDate
  printf (intercalate "," strs)
  printf "\n"

cmdPlotSums :: IO ()
cmdPlotSums = do
  runDef <- loadRunDef "./def"
  runInfos <- loadRunInfos "./runs"

  let minDate = minimum $ map rDate runInfos
      maxDate = maximum $ map rDate runInfos

  putStrLn "set timefmt \"%Y-%m-%d-%H:%M:%S\""
  putStrLn "set xdata time"
  printf "set xrange [\"%s\" : \"%s\"]\n"
    (formatTime defaultTimeLocale "%Y-%m-%d-%H:%M:%S" (posixSecondsToUTCTime (msecToPosix minDate)))
    (formatTime defaultTimeLocale "%Y-%m-%d-%H:%M:%S" (posixSecondsToUTCTime (msecToPosix maxDate)))

  let maxTime = unAbsolute $ maximum $ mapMaybe maybeTime $ concat $ map (Map.elems . rRun) runInfos
  putStrLn "set ydata time"
  printf "set yrange [\"1970-01-01-00:00:00.0\" : \"1970-01-01-%s\"]\n" (formatMsec maxTime)

  printf "$data << EOD\n"
  forM_ (zip [1..] runInfos) $ \(j, runInfo) -> do
    printf "%s 1970-01-01-%s\n" (formatTime defaultTimeLocale "%Y-%m-%d-%H:%M:%S" (posixSecondsToUTCTime (msecToPosix (rDate runInfo))))
                   (formatMsec (unAbsolute (maximum (mapMaybe maybeTime (Map.elems (rRun runInfo))))))
  printf "EOD\n"
  printf "plot '$data' using 1:2 title \"%s - %s\" with lines\n" (rdTitle runDef) (rdGoal runDef)
