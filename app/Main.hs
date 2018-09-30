{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Main where

import Control.Concurrent
import Control.Concurrent.MVar
import Control.Monad
import Control.Monad.State
import Control.Monad.Trans
import qualified Data.ByteString.Lazy as BL
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

import Export.SplitsIO
import qualified GlobalHotkeys as GH
import qualified Plotter.ChartJS as ChartJS
import qualified Plotter.GnuPlot as GnuPlot
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
  <> command "export"   (info exportP
                              (progDesc "Export to other file formats"))
  ) <|> pure cmdRun

plotP :: Parser (IO ())
plotP = hsubparser
  (  command "runs"     (info (cmdPlotRuns <$> plotterArgument)
                              (progDesc "Overlay all runs on top of each other"))
  <> command "sums"     (info (cmdPlotSums <$> plotterArgument)
                              (progDesc "Plot the sum times in chronological order"))
  )

exportP :: Parser (IO ())
exportP = hsubparser
  (  command "splitsio" (info (cmdExportSplitsIO <$> runNumberArgument)
                              (progDesc "Splits.io exchange format"))
  )

data Plotter = GnuPlot | ChartJS

plotterArgument = option (maybeReader reader) (short 'p' <> long "plotter" <> metavar "PLOTTER" <> value GnuPlot)
  where
    reader "gnuplot" = Just GnuPlot
    reader "chartjs" = Just ChartJS
    reader _ = Nothing

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

data SplitInfo = SplitInfo
  { splitName :: Text
  , splitAbsTime :: Maybe (Absolute Msec)
  , splitRelTime :: Maybe (Relative Msec)
  , splitAbsBest :: Maybe (Absolute Msec)
  , splitRelBest :: Maybe (Relative Msec)
  }

drawRun :: Msec -> LZ.Zipper SplitInfo -> CairoWidget (V Dim) (V Dim) (StyleT IO)
drawRun t splitInfos = alignLeft (fixh 100 (fixw 500 (scale mainTime))) `topOf` runList
  where
    mainTime :: CairoWidget (F Dim) (F Dim) (StyleT IO)
    mainTime = text (formatMsec (Absolute t))

    runList :: CairoWidget (V Dim) (V Dim) (StyleT IO)
    runList = listOf (fmap (withSelectionBox drawSplit) (LZ.duplicatez splitInfos))

    withSelectionBox f name True = box (f name)
    withSelectionBox f name False = f name

    drawSplit :: LZ.Zipper SplitInfo -> CairoWidget (V Dim) (F Dim) (StyleT IO)
    drawSplit splitz =
      let split = LZ.cursor splitz
          absDiff = subtract <$> splitAbsBest split <*> splitAbsTime split
          relDiff = subtract <$> splitRelBest split <*> splitRelTime split
          col = splitColor (splitAbsTime split) (splitAbsBest split) (splitRelTime split) (splitRelBest split)
      in tabularH
        [ (0.5, alignLeft (text (unpack (splitName split))))
        , (0.2, alignLeft (drawTime (text <$> formatMsec <$> splitAbsTime split)))
        , (0.3, alignLeft (withStyling (color1 col) (drawTime (text <$> formatMsecDiff <$> absDiff))))
        ]

    drawTime = fromMaybe (text "---")

    isBetter (Just time) (Just bestTime) = time < bestTime
    isBetter (Just time) Nothing = True
    isBetter _ _ = False
    splitColor absTime bestAbsTime relTime bestRelTime
      | isBetter relTime bestRelTime = RGB 1 1 0.5 
      | isBetter absTime bestAbsTime = RGB 0.5 1 0.5
      | otherwise = RGB 1 0.5 0.5

makeSplitInfoZipper :: Either Bool Text -> RunDef -> [RunInfo] -> RunInfo -> LZ.Zipper SplitInfo
makeSplitInfoZipper cur def runInfos run = fmap makeSplitInfo (LZ.duplicatez splitZipper)
  where
    (doneSplits, currentSplits, todoSplits) = case cur of
      Left False -> ([], [], rdSplits def)
      Left True -> (rdSplits def, [], [])
      Right cur' -> let (a, b) = break (== cur') (rdSplits def) in (a, [head b], tail b)

    splitZipper = LZ.Zip (reverse doneSplits) (currentSplits ++ todoSplits)

    makeSplitInfo splitz =
      let name = LZ.cursor splitz
      in SplitInfo
        { splitName = name
        , splitAbsTime = lookupTime name (rRun run)
        , splitRelTime = lookupTime name (toRelative (rRun run))
        , splitAbsBest = lookupTime name (bestRun (map rRun runInfos))
        , splitRelBest = lookupTime name (bestSplits (map (toRelative . rRun) runInfos))
        }

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
    liftIO $ draw $ drawRun 0 $ makeSplitInfoZipper (Left False) runDef runs beforeStart

    liftIO $ takeMVar sem
    start <- liftIO $ posixToMsec <$> getPOSIXTime

    forM_ (rdSplits runDef) $ \name -> do
      run <- get

      r <- liftIO $ loopMVar sem $ do
        t <- (subtract start) <$> posixToMsec <$> getPOSIXTime
        let run' = run { rRun = Map.insert name (ValidTime (Absolute t)) (rRun run) }
        draw $ drawRun t $ makeSplitInfoZipper (Right name) runDef runs run'

      split <- liftIO $
        if r
        then ValidTime <$> Absolute <$> subtract start <$> posixToMsec <$> getPOSIXTime
        else InvalidTime <$> Absolute <$> subtract start <$> posixToMsec <$> getPOSIXTime
      modify $ \run -> run { rRun = Map.insert name split (rRun run) }

    end <- liftIO $ posixToMsec <$> getPOSIXTime
    return (end - start)

  draw $ drawRun t (makeSplitInfoZipper (Left True) runDef runs run) `topOf` alignLeft (text "Space to save, escape to discard.")

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
  let relativeRun = toRelative absoluteRun
  let bestRelative = bestSplits $ map (toRelative . rRun) runs

  forM_ (rdSplits runDef) $ \name -> do
    let absoluteTime = lookupTime name absoluteRun
    let relativeTime = lookupTime name relativeRun
    let absoluteDiff = subtract <$> lookupTime name (bestRun (rRun <$> runs)) <*> absoluteTime
    let relativeDiff = subtract <$> lookupTime name bestRelative <*> relativeTime
    printf "%30s %10s %10s %10s %10s\n"
      name
      (fromMaybe "---" (fmap formatMsec absoluteTime))
      (fromMaybe "---" (fmap formatMsec relativeTime))
      (fromMaybe "---" (fmap formatMsecDiff absoluteDiff))
      (fromMaybe "---" (fmap formatMsecDiff relativeDiff))

cmdPlotRuns :: Plotter -> IO ()
cmdPlotRuns plotter = do
  runDef <- loadRunDef "./def"
  runInfos <- loadRunInfos "./runs"
  case plotter of
    GnuPlot -> putStrLn $ unpack $ GnuPlot.plotRuns runDef runInfos
    ChartJS -> putStrLn $ unpack $ ChartJS.plotRuns runDef runInfos

cmdPlotSums :: Plotter -> IO ()
cmdPlotSums plotter = do
  runDef <- loadRunDef "./def"
  runInfos <- loadRunInfos "./runs"
  case plotter of
    GnuPlot -> putStrLn $ unpack $ GnuPlot.plotSums runDef runInfos
    ChartJS -> putStrLn $ unpack $ ChartJS.plotSums runDef runInfos

cmdExportSplitsIO :: Int -> IO ()
cmdExportSplitsIO n = do
  runDef <- loadRunDef "./def"
  runInfos <- loadRunInfos "./runs"

  BL.putStr (exportSplitsIO runDef (runInfos !! n) runInfos)
