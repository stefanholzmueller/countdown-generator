module Duration where
  
import Prelude

import Data.Int (fromNumber, toNumber)
import Data.Maybe (fromJust)
import Data.Time.Duration (class Duration, Milliseconds(..))
import Data.Tuple (Tuple(..))
import Math (remainder, trunc)
import Partial.Unsafe (unsafePartial)


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








