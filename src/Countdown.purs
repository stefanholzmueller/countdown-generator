module Countdown where

import Prelude

import Config as C
import Control.Apply (lift2)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Now (NOW, locale, now)
import Data.DateTime (DateTime(..), adjust, date, diff, modifyTime, weekday)
import Data.DateTime.Instant (toDateTime)
import Data.DateTime.Locale (Locale(..))
import Data.Enum (fromEnum)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..), fromJust, fromMaybe)
import Data.Time.Duration (Days(..), Milliseconds)
import Duration as D
import Partial.Unsafe (unsafePartial)


type CountdownResult = Maybe D.DurationComponents

countdownEnd :: C.Config -> DateTime -> DateTime
countdownEnd config now = case config of 
  (C.Weekly weekly) -> let dayOfWeek = fromEnum $ weekday $ date now
                           dayDiff   = Days $ toNumber (weekly.startDayOfWeek - dayOfWeek)
                           startDate = unsafePartial $ fromJust $ adjust dayDiff now
                        in modifyTime (const weekly.startTime) startDate
  (C.Fixed fixed) -> DateTime fixed.startDate fixed.startTime

countdown :: C.Config -> DateTime -> CountdownResult
countdown config now = if end < now then Nothing else Just multiUnitDuration
  where 
  end = countdownEnd config now
  difference :: Milliseconds
  difference = diff end now
  multiUnitDuration = D.durationComponents difference

currentLocalTime :: forall eff. Eff (now :: NOW | eff) DateTime
currentLocalTime = map (fromMaybe bottom) effMaybeDateTime
  where
  effMaybeDateTime = lift2 adjust offset (map toDateTime now)
  offset = map (\(Locale _ min) -> negate min) locale
