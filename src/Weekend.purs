module Weekend where

import Prelude

import Control.Apply (lift2)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Now (NOW, locale, now)
import Data.DateTime (DateTime, adjust, date, hour, minute, second, time, weekday)
import Data.DateTime.Instant (toDateTime)
import Data.DateTime.Locale (Locale(..))
import Data.Either (either)
import Data.Enum (fromEnum)
import Data.Formatter.DateTime (formatDateTime)
import Data.Maybe (Maybe(..), fromMaybe, isNothing)
import Data.Time.Duration (Minutes)


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

type Duration = { days :: Int, hours :: Int, minutes :: Int, seconds :: Int }

durationTillWeekend :: forall eff. Eff (now :: NOW | eff) (Maybe Duration)
durationTillWeekend = map testWeekend currentLocalTime
  where
  testWeekend dateTime = 
    let dayOfWeek = fromEnum $ weekday $ date dateTime
        hourOfDay = fromEnum $ hour    $ time dateTime
        minutes   = fromEnum $ minute  $ time dateTime
        seconds   = fromEnum $ second  $ time dateTime
    in if dayOfWeek > 5 then Nothing else if hourOfDay > 16 then Nothing else Just { days: 5 - dayOfWeek, hours: 0, minutes: 0, seconds: 0 }













