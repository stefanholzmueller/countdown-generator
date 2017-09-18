module Weekend where

import Prelude

import Control.Apply (lift2)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Now (NOW, locale, now)
import Data.DateTime (DateTime, adjust, date, diff, modifyTime, setHour, setMinute, setSecond, weekday)
import Data.DateTime.Instant (toDateTime)
import Data.DateTime.Locale (Locale(..))
import Data.Either (either)
import Data.Enum (fromEnum, toEnum)
import Data.Formatter.DateTime (formatDateTime)
import Data.Int (floor, toNumber)
import Data.Maybe (Maybe(..), fromMaybe, isNothing)
import Data.Newtype (unwrap)
import Data.Time.Duration (Days(..), Hours, Minutes, Seconds, fromDuration, toDuration)


weekendStartDayOfWeek :: Int
weekendStartDayOfWeek = 5

weekendStartHour :: Int
weekendStartHour = 17

offset :: forall eff. Eff (now :: NOW | eff) Minutes
offset = map (\(Locale _ min) -> negate min) locale

currentLocalTime :: forall eff. Eff (now :: NOW | eff) DateTime
currentLocalTime = map (fromMaybe bottom) effMaybeDateTime
  where
  effMaybeDateTime = lift2 adjust offset (map toDateTime now)

formattedCurrentTime :: forall eff. Eff (now :: NOW | eff) String
formattedCurrentTime = map toString currentLocalTime
  where
  toString dateTime = let workaroundForWeekday = show $ weekday $ date dateTime
                          formattedDate = (formatDateTime "HH:mm" >>> either (const "ERROR") id) dateTime
                       in workaroundForWeekday <> ", " <> formattedDate

isWeekend :: forall eff. Eff (now :: NOW | eff) Boolean
isWeekend = map isNothing durationTillWeekend

type Duration = { days :: Days, hours :: Hours, minutes :: Minutes, seconds :: Seconds }

durationTillWeekend :: forall eff. Eff (now :: NOW | eff) (Maybe Duration)
durationTillWeekend = map testWeekend currentLocalTime
  where
  testWeekend now = 
    let dayOfWeek = fromEnum $ weekday $ date now
        dayDiff   = Days $ toNumber (weekendStartDayOfWeek - dayOfWeek)
        startDate = fromMaybe bottom $ adjust dayDiff now
        startDatetime = modifyTime ((setHour $ fromMaybe bottom $ toEnum weekendStartHour) >>> (setMinute $ fromMaybe bottom $ toEnum 0) >>> (setSecond $ fromMaybe bottom $ toEnum 0)) startDate
        duration :: Days
        duration  = diff startDatetime now
    in if startDatetime < now
       then Nothing
       else Just { days: duration
                 , hours: toDuration $ fromDuration duration
                 , minutes: toDuration $ fromDuration duration
                 , seconds: toDuration $ fromDuration duration
                 }





