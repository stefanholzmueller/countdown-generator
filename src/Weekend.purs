module Weekend where

import Prelude

import Control.Apply (lift2)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Now (NOW, locale, now)
import Countdown as C
import Data.DateTime (DateTime, Time(..), adjust, date, diff, weekday)
import Data.DateTime.Instant (toDateTime)
import Data.DateTime.Locale (Locale(..))
import Data.Either (either)
import Data.Enum (toEnum)
import Data.Formatter.DateTime (formatDateTime)
import Data.Maybe (Maybe(..), fromJust, fromMaybe, isNothing)
import Duration as D
import Partial.Unsafe (unsafePartial)


config :: C.Config
config = C.Weekly { dayOfWeek: 5
                  , startTime: unsafePartial $ fromJust $ Time <$> toEnum 17 <*> toEnum 0 <*> toEnum 0 <*> toEnum 0
                  }

currentLocalTime :: forall eff. Eff (now :: NOW | eff) DateTime
currentLocalTime = map (fromMaybe bottom) effMaybeDateTime
  where
  effMaybeDateTime = lift2 adjust offset (map toDateTime now)
  offset = map (\(Locale _ min) -> negate min) locale

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
    let countdownEnd = C.countdownEnd config now
     in if countdownEnd < now
        then Nothing
        else let difference :: D.ItemizedDuration
                 difference = diff countdownEnd now
                 (D.Itemized itemized) = difference
                 formatTime t = if t < 10 then "0" <> show t else show t
                 dd = case itemized.days of
                        0 -> ""
                        1 -> "1 day and "
                        d -> show d <> " days and "
                 hh = formatTime itemized.hours
                 mm = formatTime itemized.minutes
                 ss = formatTime itemized.seconds
              in Just $ dd <> hh <> ":" <> mm <> ":" <> ss
