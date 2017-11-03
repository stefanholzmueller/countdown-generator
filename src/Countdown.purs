module Countdown where

import Prelude

import Control.Apply (lift2)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Now (NOW, locale, now)
import Data.DateTime (Date, DateTime(..), Month(..), Time(..), adjust, canonicalDate, date, modifyTime, weekday)
import Data.DateTime.Instant (toDateTime)
import Data.DateTime.Locale (Locale(..))
import Data.Enum (fromEnum, toEnum)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..), fromJust, fromMaybe)
import Data.Time.Duration (Days(..))
import Partial.Unsafe (unsafePartial)


data Config = Weekly { startDayOfWeek :: Int, startTime :: Time, nowFormat :: String }
            | Fixed { startDate :: Date, startTime :: Time }

config :: Config
config = Fixed { startDate: unsafePartial $ fromJust $ canonicalDate <$> toEnum 2017 <*> (Just November) <*> toEnum 17
               , startTime: unsafePartial $ fromJust $ Time <$> toEnum 18 <*> toEnum 0 <*> toEnum 0 <*> toEnum 0
               }

countdownEnd :: Config -> DateTime -> DateTime
countdownEnd config now = case config of 
  (Weekly weekly) -> let dayOfWeek = fromEnum $ weekday $ date now
                         dayDiff   = Days $ toNumber (weekly.startDayOfWeek - dayOfWeek)
                         startDate = unsafePartial $ fromJust $ adjust dayDiff now
                      in modifyTime (const weekly.startTime) startDate
  (Fixed fixed) -> DateTime fixed.startDate fixed.startTime

isEventReached :: Config -> DateTime -> Boolean
isEventReached config now = countdownEnd config now < now

currentLocalTime :: forall eff. Eff (now :: NOW | eff) DateTime
currentLocalTime = map (fromMaybe bottom) effMaybeDateTime
  where
  effMaybeDateTime = lift2 adjust offset (map toDateTime now)
  offset = map (\(Locale _ min) -> negate min) locale
