{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
module Types where

import Control.Exception
import Data.Aeson
import Data.Aeson.Types
import qualified Data.ByteString.Lazy as BL
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

data Time a = ValidTime a | InvalidTime
  deriving (Eq, Show)

fromTime :: a -> Time a -> a
fromTime _ (ValidTime x) = x
fromTime x InvalidTime = x

instance Functor Time where
  fmap f (ValidTime x) = ValidTime (f x)
  fmap f InvalidTime = InvalidTime

instance Applicative Time where
  pure = ValidTime
  f <*> a = case (f, a) of
    (ValidTime f', ValidTime a') -> ValidTime (f' a')
    _ -> InvalidTime

instance Monad Time where
  a >>= f = case a of
    ValidTime a' -> f a'
    InvalidTime -> InvalidTime

newtype Absolute a = Absolute { unAbsolute :: a }
  deriving (Eq, Functor, Ord, Show)
newtype Relative a = Relative { unRelative :: a }
  deriving (Eq, Functor, Ord, Show)

instance (Ord a) => Ord (Time a) where
  compare a b = case (a, b) of
                  (InvalidTime, InvalidTime) -> EQ
                  (InvalidTime, b') -> GT
                  (a', InvalidTime) -> LT
                  (ValidTime a', ValidTime b') -> compare a' b'

instance FromJSON (Time (Absolute Msec)) where
  parseJSON Null = return InvalidTime
  parseJSON (Number n) = return (ValidTime (Absolute (floor n)))
  parseJSON s = typeMismatch "Time" s

instance ToJSON (Time (Absolute Msec)) where
  toJSON InvalidTime = Null
  toJSON (ValidTime (Absolute n)) = Number (fromIntegral n)

type Run = Map Text (Time (Absolute Msec))

lookupTime :: Text -> Map Text (Time a) -> Time a
lookupTime name = Map.findWithDefault InvalidTime name

runToList :: [Text] -> Run -> [Time (Absolute Msec)]
runToList names run = map (flip lookupTime run) names

data RunInfo = RunInfo
  { rDate :: Msec
  , rRun :: Run
  } deriving (Show)

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
    f (False, last) (InvalidTime : ss) = InvalidTime : f (False, last) ss
    f (False, last) (ValidTime (Absolute n) : ss) = InvalidTime : f (True, n) ss
    f (True,  last) (ValidTime (Absolute n) : ss) = ValidTime (Relative (n - last)) : f (True, n) ss
    f (True,  last) (InvalidTime : ss) = InvalidTime : f (False, last) ss

best :: (Ord a, Ord k) => [Map k a] -> Map k a
best = foldr (Map.unionWith min) Map.empty

toRelative :: [Text] -> Map Text (Time (Absolute Msec)) -> Map Text (Time (Relative Msec))
toRelative names run = Map.fromList $ zip names $ relativeTimes $ runToList names run

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
