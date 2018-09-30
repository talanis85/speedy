{-# LANGUAGE OverloadedStrings #-}
module Export.SplitsIO where

import Data.Aeson
import qualified Data.ByteString.Lazy as BL
import Data.List
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Maybe
import Data.Text.Lazy (Text)
import qualified Data.Vector as Vector

import Types

exportSplitsIO :: RunDef -> RunInfo -> [RunInfo] -> BL.ByteString
exportSplitsIO runDef runInfo runInfos = encode (SplitsIO runDef runInfo runInfos)

data SplitsIO = SplitsIO RunDef RunInfo [RunInfo]

instance ToJSON SplitsIO where
  toJSON (SplitsIO runDef runInfo runInfos) = object
    [ "_schemaVersion" .= ("v1.0.0" :: Text)
    , "timer" .= object
      [ "shortname" .= ("speedy" :: Text)
      , "longname" .= ("speedy" :: Text)
      , "version" .= ("0.0.1" :: Text)
      ]
    , "game" .= object [ "longname" .= rdTitle runDef ]
    , "category" .= object [ "longname" .= rdGoal runDef ]
    , "segments" .= Array (Vector.fromList (map mkSegment (sortOn snd (Map.toList (rRun runInfo)))))
    ]
    where
      mkSegment (name, t) = object $
        [ "name" .= name
        , "endedAt" .= object [ "realtimeMS" .= Number (fromIntegral (unAbsolute (fromTime t))) ]
        , "isSkipped" .= Bool (case t of
            InvalidTime _ -> True
            ValidTime _ -> False
            )
        ]
        ++ bestDuration name
      bestDuration name = case lookupTime name (bestSplits (map (toRelative . rRun) runInfos)) of
        Nothing -> []
        Just t' -> [ "bestDuration" .= object [ "realtimeMS" .= Number (fromIntegral (unRelative t')) ] ]
