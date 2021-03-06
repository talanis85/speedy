{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
module Types where

import Control.Exception
import Data.Aeson
import Data.Aeson.Types
import qualified Data.ByteString.Lazy as BL
import Data.List
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as Text
import qualified Data.Text.Lazy.IO as Text
import Data.Time.Clock
import Data.Time.Clock.POSIX
import Data.Time.Format
import System.Directory
import Text.Printf

data RunDef = RunDef
  { rdTitle :: Text
  , rdGoal :: Text
  , rdSplits :: [Text]
  } deriving (Show)

instance FromJSON RunDef where
  parseJSON = withObject "RunDef" $ \v -> RunDef
    <$> v .: "title"
    <*> v .: "goal"
    <*> v .: "splits"

instance ToJSON RunDef where
  toJSON rd = object [ "title" .= rdTitle rd, "goal" .= rdGoal rd, "splits" .= rdSplits rd ]

type Msec = Integer

posixToMsec :: POSIXTime -> Msec
posixToMsec = (`div` (10^9)) . diffTimeToPicoseconds . realToFrac

msecToPosix :: Msec -> POSIXTime
msecToPosix = realToFrac . picosecondsToDiffTime . (* (10^9))

timeToMsec :: Integer -> Integer -> Integer -> Integer -> Msec
timeToMsec h m s ms = h * 60 * 60 * 1000 + m * 60 * 1000 + s * 1000 + ms

data Time a = ValidTime a | InvalidTime a
  deriving (Eq, Show)

fromTime :: Time a -> a
fromTime (ValidTime x) = x
fromTime (InvalidTime x) = x

fromValidTime :: a -> Time a -> a
fromValidTime _ (ValidTime x) = x
fromValidTime x (InvalidTime _) = x

maybeTime :: Time a -> Maybe a
maybeTime (ValidTime x) = Just x
maybeTime (InvalidTime x) = Nothing

instance Functor Time where
  fmap f (ValidTime x) = ValidTime (f x)
  fmap f (InvalidTime x) = InvalidTime (f x)

instance Applicative Time where
  pure = ValidTime
  f <*> a = case (f, a) of
    (ValidTime f', ValidTime a') -> ValidTime (f' a')
    (InvalidTime f', ValidTime a') -> InvalidTime (f' a')
    (ValidTime f', InvalidTime a') -> InvalidTime (f' a')
    (InvalidTime f', InvalidTime a') -> InvalidTime (f' a')

instance Monad Time where
  a >>= f = case a of
    ValidTime a' -> f a'
    InvalidTime a' -> case f a' of
      ValidTime x -> InvalidTime x
      InvalidTime x -> InvalidTime x

newtype Absolute a = Absolute { unAbsolute :: a }
  deriving (Eq, Functor, Num, Ord, Show)
newtype Relative a = Relative { unRelative :: a }
  deriving (Eq, Functor, Num, Ord, Show)

class FormatMsec a where
  formatMsec :: a -> String
  formatMsecDiff :: a -> String

instance FormatMsec (Absolute Msec) where
  formatMsec (Absolute msec) = formatMsec' msec
  formatMsecDiff (Absolute msec) = formatMsecDiff' msec

instance FormatMsec (Relative Msec) where
  formatMsec (Relative msec) = formatMsec' msec
  formatMsecDiff (Relative msec) = formatMsecDiff' msec

formatMsec' :: Msec -> String
formatMsec' msec = printf "%s%02d:%02d:%02d.%01d" (sgn :: String) h m s ds
  where (sgn, msec') = ((if msec < 0 then "-" else ""), abs msec)
        (s', ms) = msec' `divMod` 1000
        (m', s) = s' `divMod` 60
        (h, m) = m' `divMod` 60
        ds = ms `div` 100

formatMsecDiff' :: Msec -> String
formatMsecDiff' msec = printf "%s%d.%01d" (sgn :: String) s ds
  where (sgn, msec') = ((if msec < 0 then "-" else "+"), abs msec)
        (s, ms) = msec' `divMod` 1000
        ds = ms `div` 100

instance (Ord a) => Ord (Time a) where
  compare a b = compare (fromTime a) (fromTime b)

newtype Best a = Best { unBest :: a }

instance (Eq a) => Eq (Best (Time a)) where
  Best (InvalidTime a) == Best (InvalidTime b) = a == b
  Best (ValidTime a) == Best (ValidTime b) = a == b
  _ == _ = False

instance (Ord a) => Ord (Best (Time a)) where
  compare (Best a) (Best b) = case (a, b) of
                  (InvalidTime a', InvalidTime b') -> compare b' a'
                  (InvalidTime a', ValidTime b') -> LT
                  (ValidTime a', InvalidTime b') -> GT
                  (ValidTime a', ValidTime b') -> compare b' a'

instance FromJSON (Time (Absolute Msec)) where
  parseJSON (Object ob) = InvalidTime <$> Absolute <$> ob .: "invalid"
  parseJSON (Number n) = return (ValidTime (Absolute (floor n)))
  parseJSON s = typeMismatch "Time" s

instance ToJSON (Time (Absolute Msec)) where
  toJSON (InvalidTime (Absolute n))= object [("invalid", Number (fromIntegral n))]
  toJSON (ValidTime (Absolute n)) = Number (fromIntegral n)

type Run = Map Text (Time (Absolute Msec))

lookupTime :: Text -> Map Text (Time a) -> Maybe a
lookupTime name run = Map.findWithDefault Nothing name (maybeTime <$> run)

data RunInfo = RunInfo
  { rDate :: Msec
  , rRun :: Run
  } deriving (Show)

runTime :: Map Text (Time (Absolute Msec)) -> Time (Absolute Msec)
runTime run = case Map.toList run of
  [] -> error "Broken (empty) run"
  xs -> last $ sort $ map snd xs

instance FromJSON RunInfo where
  parseJSON = withObject "RunInfo" $ \v -> RunInfo
    <$> v .: "date"
    <*> v .: "run"

instance ToJSON RunInfo where
  toJSON ri = object [ "date" .= rDate ri, "run" .= rRun ri ]

initRunInfo :: POSIXTime -> RunInfo
initRunInfo date = RunInfo { rDate = posixToMsec date, rRun = Map.empty }

loadRunDef :: Text -> IO RunDef
loadRunDef file = do
  f <- BL.readFile (Text.unpack file)
  case decode f of
    Nothing -> throw (userError "Invalid run definition")
    Just r -> return r

loadRunInfos :: Text -> IO [RunInfo]
loadRunInfos file = do
  ex <- doesFileExist (Text.unpack file)
  if ex
     then do
       f <- BL.readFile (Text.unpack file)
       case decode f of
         Nothing -> throw (userError "Invalid run database")
         Just r -> return r
     else return []

writeRunInfos :: Text -> [RunInfo] -> IO ()
writeRunInfos file runs = BL.writeFile (Text.unpack file) (encode runs)

-- Statistics

relativeTimes :: [Time (Absolute Msec)] -> [Time (Relative Msec)]
relativeTimes = f (True, 0) 
  where
    f (valid, last) [] = []
    f (False, last) (InvalidTime (Absolute n) : ss) = InvalidTime (Relative (n - last)) : f (False, n) ss
    f (False, last) (ValidTime (Absolute n) : ss) = InvalidTime (Relative (n - last)) : f (True, n) ss
    f (True,  last) (ValidTime (Absolute n) : ss) = ValidTime (Relative (n - last)) : f (True, n) ss
    f (True,  last) (InvalidTime (Absolute n) : ss) = InvalidTime (Relative (n - last)) : f (False, n) ss

bestSplits :: (Ord a) => [Map Text (Time (Relative a))] -> Map Text (Time (Relative a))
bestSplits runs = unBest <$> foldr (Map.unionWith max) Map.empty (fmap Best <$> runs)

bestRun :: [Map Text (Time (Absolute Msec))] -> Map Text (Time (Absolute Msec))
bestRun runs = safeLast (sortOn (Best . runTime) runs)
  where safeLast [] = Map.empty
        safeLast (x:xs) = last (x:xs)

toRelative :: Map Text (Time (Absolute Msec)) -> Map Text (Time (Relative Msec))
toRelative run =
  let runList = sortOn snd $ Map.toList run
      names = map fst runList
      values = map snd runList
  in Map.fromList $ zip names $ relativeTimes values
