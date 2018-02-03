module Config where
  
import Prelude

import Data.Date (Date, Month(..), canonicalDate)
import Data.Enum (toEnum)
import Data.Maybe (Maybe(..), fromJust)
import Data.Time (Time(..))
import Partial.Unsafe (unsafePartial)


data Config = Weekly { startDayOfWeek :: Int, startTime :: Time }
            | Fixed { startDate :: Date, startTime :: Time }

config :: Config
config = Fixed { startDate: unsafePartial $ fromJust $ canonicalDate <$> toEnum 2017 <*> (Just November) <*> toEnum 17
               , startTime: unsafePartial $ fromJust $ Time <$> toEnum 17 <*> toEnum 0 <*> toEnum 0 <*> toEnum 0
               }
