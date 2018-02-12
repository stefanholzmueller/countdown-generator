module Config where
  
import Prelude

import Data.Date (Date, Month(..), canonicalDate, day, month, year)
import Data.Either (Either(..))
import Data.Enum (fromEnum, toEnum)
import Data.Maybe (Maybe(..), fromJust)
import Data.Time (Time(..), hour, minute, second)
import Data.URI (Query(..), URI(..))
import Partial.Unsafe (unsafePartial)


data Config = Weekly { startDayOfWeek :: Int, startTime :: Time }
            | Fixed { startDate :: Date, startTime :: Time }

instance showConfig :: Show Config -- for debugging
  where show config = serialize config

config :: Config
config = Fixed { startDate: unsafePartial $ fromJust $ canonicalDate <$> toEnum 2017 <*> (Just November) <*> toEnum 17
               , startTime: unsafePartial $ fromJust $ Time <$> toEnum 17 <*> toEnum 0 <*> toEnum 0 <*> toEnum 0
               }

serialize :: Config -> String
serialize config = case config of
  (Weekly { startDayOfWeek, startTime }) -> "?type=weekly&startDayOfWeek=" <> show startDayOfWeek <> timeParam startTime
  (Fixed { startDate, startTime }) -> "?type=fixed" <> dateParam startDate <> timeParam startTime
  where
  dateParam :: Date -> String
  dateParam date = "&year=" <> (show <<< fromEnum <<< year) date <> "&month=" <> (show <<< fromEnum <<< month) date <> "&day=" <> (show <<< fromEnum <<< day) date
  timeParam :: Time -> String
  timeParam time = "&hour=" <> (show <<< fromEnum <<< hour) time <> "&minute=" <> (show <<< fromEnum <<< minute) time <> "&second=" <> (show <<< fromEnum <<< second) time

deserialize :: URI -> Either String Config
deserialize (URI _ _ (Just (Query params)) _) = Right config
deserialize _ = Left "URI has no query parameters"
