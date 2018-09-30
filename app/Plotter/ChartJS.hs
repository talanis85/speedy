{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Plotter.ChartJS where

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
import Text.Shakespeare.Text

import Types

plotSums :: RunDef -> [RunInfo] -> Text
plotSums runDef runInfos = [lt|
<html>
  <head>
    <script src="https://cdnjs.cloudflare.com/ajax/libs/moment.js/2.13.0/moment.min.js"></script>
    <script src="https://cdnjs.cloudflare.com/ajax/libs/moment-duration-format/2.2.2/moment-duration-format.min.js"></script>
    <script src="https://cdnjs.cloudflare.com/ajax/libs/Chart.js/2.7.2/Chart.min.js"></script>
  </head>
  <body>
    <div class="chart-container" style="position: relative; height:100%; width:100%">
      <canvas id="myChart"></canvas>
    </div>
    <script>
      var ctx = document.getElementById("myChart").getContext('2d');
      var myChart = new Chart(ctx, {
        type: 'line',
        data: {
          datasets: [
            {
              label: '#{label}',
              data: [ #{dataset} ]
            }
          ]
        },
        options: {
          scales: {
            xAxes: [{
              type: 'time',
              time: {
                unit: 'day'
              }
            }],
            yAxes: [{
              ticks: {
                callback: function(value, index, ticks) {
                  return moment.duration(value).format('hh:mm:ss.SS');
                }
              }
            }]
          }
        }
      });
    </script>
  </body>
</html>
  |]
  where
    label = rdTitle runDef
    dataset = intercalate "," $ flip map (zip [1..] runInfos) $ \(j, runInfo) ->
      printf "{ x: new Date(%d), y: %d }\n"
        (rDate runInfo)
        (unAbsolute (maximum (mapMaybe maybeTime (Map.elems (rRun runInfo)))))

plotRuns :: RunDef -> [RunInfo] -> Text
plotRuns runDef runInfos = ""
