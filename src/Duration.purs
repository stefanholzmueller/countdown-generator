module Duration where
  
import Prelude

import Data.Int (fromNumber)
import Data.List (List)
import Data.Maybe (fromJust)
import Data.Time.Duration (class Duration, Milliseconds(..), convertDuration)
import Data.Tuple (Tuple(..))
import Math (remainder, trunc)
import Partial.Unsafe (unsafePartial)


formatDuration :: forall d. Duration d => List FormatterCommand -> d -> String
formatDuration format duration = "TODO"

type DurationComponents = { days :: Int
                          , hours :: Int
                          , minutes :: Int
                          , seconds :: Int
                          , ms :: Number
                          }

durationComponents :: forall d. Duration d => d -> DurationComponents
durationComponents duration = { days, hours, minutes, seconds, ms }
  where
  milliseconds :: Milliseconds
  milliseconds = convertDuration duration
  (Milliseconds ms') = milliseconds
  toInt n = unsafePartial $ fromJust $ fromNumber n
  divMod x y = Tuple (trunc $ div x y) (remainder x y)
  Tuple s' ms = divMod ms' 1000.0
  Tuple m' s  = divMod s' 60.0
  Tuple h' m  = divMod m' 60.0
  Tuple d  h  = divMod h' 24.0
  days    = toInt d  -- Number.MAX_SAFE_INTEGER / 86400000 is smaller than 2^31
  hours   = toInt h
  minutes = toInt m
  seconds = toInt s

data FormatterElement = Days
                      | Hours
                      | Minutes
                      | Seconds
                      | MillisecondsXXX
data FormatterCommand = Simple FormatterElement
                      | Pluralized2 FormatterElement (Number -> String)
                      | PadTwoDigits FormatterElement
                      | PadThreeDigits FormatterElement
                      | Literal String
