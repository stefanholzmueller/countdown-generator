module Weekend where

import Prelude

import Config (config)
import Countdown as C
import Data.DateTime (DateTime, diff)
import Data.Formatter.Interval (formatInterval)
import Data.Maybe (Maybe(..))
import Duration as D


durationTillWeekend :: DateTime -> (Maybe String)
durationTillWeekend now = 
    let countdownEnd = C.countdownEnd config now 
     in if countdownEnd < now
        then Nothing1
        else Just let difference :: D.ItemizedDuration
                      difference = diff countdownEnd now
                      x = formatInterval
                      (D.Itemized itemized) = difference
                      formatTime t = if t < 10 then "0" <> show t else show t
                      dd = case itemized.days of
                              0 -> ""
                              1 -> "1 day and "
                              d -> show d <> " days and "
                      hh = formatTime itemized.hours
                      mm = formatTime itemized.minutes
                      ss = formatTime itemized.seconds
                    in dd <> hh <> ":" <> mm <> ":" <> ss

