{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
module Types where

import Control.Exception
import Data.Aeson
import Data.Aeson.Types
import qualified Data.ByteString.Lazy as BL
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as Text
import qualified Data.Text.Lazy.IO as Text
import Data.Time.Clock
import Data.Time.Clock.POSIX
import Data.Time.Format
import GHC.Generics
import System.Directory
import Text.Printf

data RunDef = RunDef
  { rdTitle :: Text
  , rdGoal :: Text
  , rdSplits :: [Text]
  } deriving (Generic, Show, ToJSON, FromJSON)

type Msec = Integer

posixToMsec :: POSIXTime -> Msec
posixToMsec = (`div` (10^9)) . diffTimeToPicoseconds . realToFrac

timeToMsec :: Integer -> Integer -> Integer -> Integer -> Msec
timeToMsec h m s ms = h * 60 * 60 * 1000 + m * 60 * 1000 + s * 1000 + ms

data Split a = ValidSplit a | InvalidSplit
  deriving (Eq, Show)

fromSplit :: a -> Split a -> a
fromSplit _ (ValidSplit x) = x
fromSplit x InvalidSplit = x

instance Functor Split where
  fmap f (ValidSplit x) = ValidSplit (f x)
  fmap f InvalidSplit = InvalidSplit

instance Applicative Split where
  pure = ValidSplit
  f <*> a = case (f, a) of
    (ValidSplit f', ValidSplit a') -> ValidSplit (f' a')
    _ -> InvalidSplit

instance Monad Split where
  a >>= f = case a of
    ValidSplit a' -> f a'
    InvalidSplit -> InvalidSplit

newtype BestSplit = BestSplit { unBestSplit :: Split Msec }
  deriving (Eq, Show)

instance Ord BestSplit where
  compare a b = case (unBestSplit a, unBestSplit b) of
                  (InvalidSplit, InvalidSplit) -> EQ
                  (InvalidSplit, b') -> GT
                  (a', InvalidSplit) -> LT
                  (ValidSplit a', ValidSplit b') -> compare a' b'

instance FromJSON (Split Msec) where
  parseJSON Null = return InvalidSplit
  parseJSON (Number n) = return (ValidSplit (floor n))
  parseJSON s = typeMismatch "Split" s

instance ToJSON (Split Msec) where
  toJSON InvalidSplit = Null
  toJSON (ValidSplit n) = Number (fromIntegral n)

data Run = Run
  { rDate :: Msec
  , rSplitTimes :: [Split Msec]
  } deriving (Generic, Show, ToJSON, FromJSON)

emptyRun :: POSIXTime -> Run
emptyRun date = Run { rDate = posixToMsec date, rSplitTimes = [] }

loadRunDef :: Text -> IO RunDef
loadRunDef file = do
  f <- BL.readFile (Text.unpack file)
  case decode f of
    Nothing -> throw (userError "Invalid run definition")
    Just r -> return r

loadRuns :: Text -> IO [Run]
loadRuns file = do
  ex <- doesFileExist (Text.unpack file)
  if ex
     then do
       f <- BL.readFile (Text.unpack file)
       case decode f of
         Nothing -> throw (userError "Invalid run database")
         Just r -> return r
     else return []

writeRuns :: Text -> [Run] -> IO ()
writeRuns file runs = BL.writeFile (Text.unpack file) (encode runs)

migrateRuns :: [Run] -> [Run]
migrateRuns = map (\r -> r { rSplitTimes = migrateSplits 0 (rSplitTimes r) })
  where
    migrateSplits running [] = []
    migrateSplits running (ValidSplit s : ss) = ValidSplit (s + running) : migrateSplits (s + running) ss

-- Statistics

independentSplits :: [Split Msec] -> [Split Msec]
independentSplits = f (True, 0) 
  where
    f (valid, last) [] = []
    f (False, last) (_            : ss) = InvalidSplit : f (False, last) ss
    f (True,  last) (ValidSplit n : ss) = ValidSplit (n - last) : f (True, n) ss
    f (True,  last) (InvalidSplit : ss) = InvalidSplit : f (False, last) ss

bestSplits :: RunDef -> [Run] -> [Split Msec]
bestSplits def [] = replicate (length (rdSplits def)) InvalidSplit
bestSplits def runs =
  let range = [0 .. (length (rdSplits def) - 1)]
      s = map BestSplit . independentSplits . rSplitTimes
  in map (\i -> unBestSplit (minimum (map ((!! i) . s) runs))) range

bestSums :: RunDef -> [Run] -> [Split Msec]
bestSums def [] = replicate (length (rdSplits def)) InvalidSplit
bestSums def runs =
  let range = [0 .. (length (rdSplits def) - 1)]
      s = map BestSplit . rSplitTimes
  in map (\i -> unBestSplit (minimum (map ((!! i) . s) runs))) range

formatMsec :: Msec -> String
formatMsec msec = printf "%c%02d:%02d:%02d.%01d" sgn h m s ds
  where (sgn, msec') = ((if msec < 0 then '-' else ' '), abs msec)
        (s', ms) = msec' `divMod` 1000
        (m', s) = s' `divMod` 60
        (h, m) = m' `divMod` 60
        ds = ms `div` 100

formatMsecShort :: Msec -> String
formatMsecShort msec = printf "%c%d.%01d" sgn s ds
  where (sgn, msec') = ((if msec < 0 then '-' else ' '), abs msec)
        (s, ms) = msec' `divMod` 1000
        ds = ms `div` 100

formatSplit InvalidSplit = printf "%11s" ("---" :: Text)
formatSplit (ValidSplit s) = formatMsec s
