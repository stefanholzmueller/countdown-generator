module Weekend where

import Prelude

import Control.Apply (lift2)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Now (NOW, locale, now)
import Data.DateTime (DateTime, adjust, date, weekday)
import Data.DateTime.Instant (toDateTime)
import Data.DateTime.Locale (Locale(..))
import Data.Either (Either(..), either)
import Data.Formatter.DateTime (formatDateTime)
import Data.Maybe (Maybe, fromMaybe)
import Data.Time.Duration (Minutes)

offset :: forall eff. Eff (now :: NOW | eff) Minutes
offset = map (\(Locale _ min) -> negate min) locale

currentLocalTime :: forall eff. Eff (now :: NOW | eff) (Maybe DateTime)
currentLocalTime = lift2 adjust offset (map toDateTime now)

formattedCurrentTime :: forall eff. Eff (now :: NOW | eff) String
formattedCurrentTime = map toString currentLocalTime
  where
  toString maybeTime = let workaroundForWeekday = (map (\dateTime -> show $ weekday $ date dateTime) >>> fromMaybe "ERROR") maybeTime
                           formattedDate = (map (formatDateTime "HH:mm:ss") >>> fromMaybe (Left "ERROR") >>> (either (const "ERROR") id)) maybeTime
                       in workaroundForWeekday <> ", " <> formattedDate

isWeekend :: forall eff. Eff (now :: NOW | eff) Boolean
isWeekend = pure false