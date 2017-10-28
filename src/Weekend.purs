module Weekend where

import Prelude

import Control.Apply (lift2)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Now (NOW, locale, now)
import Data.DateTime (DateTime, Time(..), adjust, date, diff, modifyTime, weekday)
import Data.DateTime.Instant (toDateTime)
import Data.DateTime.Locale (Locale(..))
import Data.Either (either)
import Data.Enum (fromEnum, toEnum)
import Data.Formatter.DateTime (formatDateTime)
import Data.Int (fromNumber, toNumber)
import Data.Maybe (Maybe(..), fromJust, fromMaybe, isNothing)
import Data.Time.Duration (class Duration, Days(..), Milliseconds(..), Minutes)
import Data.Tuple (Tuple(..))
import Math (remainder, trunc)
import Partial.Unsafe (unsafePartial)


weekendStartDayOfWeek :: Int
weekendStartDayOfWeek = 5

weekendStartTime :: Time
weekendStartTime = unsafePartial $ fromJust $ Time <$> toEnum 17 <*> toEnum 0 <*> toEnum 0 <*> toEnum 0

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

durationTillWeekend :: forall eff. Eff (now :: NOW | eff) (Maybe String)
durationTillWeekend = map testWeekend currentLocalTime
  where
  testWeekend now = 
    let dayOfWeek = fromEnum $ weekday $ date now
        dayDiff   = Days $ toNumber (weekendStartDayOfWeek - dayOfWeek)
        startDate = fromMaybe bottom $ adjust dayDiff now
        countdownEnd = modifyTime (const weekendStartTime) startDate
     in if countdownEnd < now
        then Nothing
        else let difference :: ItemizedDuration
                 difference = diff countdownEnd now
                 (Itemized itemized) = difference
                 formatTime t = if t < 10 then "0" <> show t else show t
                 dd = case itemized.days of
                        0 -> ""
                        1 -> "1 day and "
                        d -> show d <> " days and "
                 hh = formatTime itemized.hours
                 mm = formatTime itemized.minutes
                 ss = formatTime itemized.seconds
              in Just $ dd <> hh <> ":" <> mm <> ":" <> ss


data ItemizedDuration = Itemized { days :: Int
                                 , hours :: Int
                                 , minutes :: Int
                                 , seconds :: Int
                                 , ms :: Number
                                 }

instance durationItemized :: Duration ItemizedDuration where
  fromDuration (Itemized { days, hours, minutes, seconds, ms }) =
    Milliseconds (toNumber days * msInDay + toNumber hours * msInHour + toNumber minutes * msInMinute + toNumber seconds * msInSecond + ms)
    where msInSecond = 1000.0
          msInMinute = 60.0 * msInSecond
          msInHour = 60.0 * msInMinute
          msInDay = 24.0 * msInHour
  toDuration (Milliseconds ms') = Itemized { days, hours, minutes, seconds, ms }
    where toInt n = unsafePartial $ fromJust $ fromNumber n
          divMod x y = Tuple (trunc $ div x y) (remainder x y)
          Tuple s' ms = divMod ms' 1000.0
          Tuple m' s  = divMod s' 60.0
          Tuple h' m  = divMod m' 60.0
          Tuple d  h  = divMod h' 24.0
          days    = toInt d  -- Number.MAX_SAFE_INTEGER / 86400000 is smaller than 2^31
          hours   = toInt h
          minutes = toInt m
          seconds = toInt s
