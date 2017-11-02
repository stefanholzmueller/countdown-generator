module Countdown where

import Prelude

import Data.DateTime (DateTime, Time, adjust, date, modifyTime, weekday)
import Data.Enum (fromEnum)
import Data.Int (toNumber)
import Data.Maybe (fromJust)
import Data.Time.Duration (Days(..))
import Partial.Unsafe (unsafePartial)


data Config = Weekly { startDayOfWeek :: Int, startTime :: Time, nowFormat :: String }
  --          | Instant { startDate :: Date, startTime :: Time }

countdownEnd :: Config -> DateTime -> DateTime
countdownEnd config now = case config of 
  (Weekly weekly) -> let dayOfWeek = fromEnum $ weekday $ date now
                         dayDiff   = Days $ toNumber (weekly.startDayOfWeek - dayOfWeek)
                         startDate = unsafePartial $ fromJust $ adjust dayDiff now
                      in modifyTime (const weekly.startTime) startDate
