module Config where
  
import Prelude

import Control.Alt ((<|>))
import Data.Date (Date, canonicalDate, day, month, year)
import Data.Either (Either(..), note)
import Data.Enum (class BoundedEnum, fromEnum, toEnum)
import Data.Int (fromString)
import Data.List (List, find)
import Data.Maybe (Maybe(..))
import Data.Time (Time(..), hour, minute, second)
import Data.Tuple (Tuple, fst, snd)
import Data.URI (Query(..), URI(..))
import Global (encodeURIComponent)


data Config = Config { event :: String, prefix :: String, startConfig :: StartConfig }

data StartConfig = Weekly { startDayOfWeek :: Int, startTime :: Time }
                 | Fixed { startDate :: Date, startTime :: Time }

instance showConfig :: Show Config -- for debugging
  where show config = serialize config

serialize :: Config -> String
serialize (Config { event, prefix, startConfig}) = "?event=" <> encodeURIComponent event <> "&prefix=" <> encodeURIComponent prefix <> case startConfig of
  (Weekly { startDayOfWeek, startTime }) -> "&type=weekly&startDayOfWeek=" <> show startDayOfWeek <> timeParams startTime
  (Fixed { startDate, startTime })       -> "&type=fixed" <> dateParams startDate <> timeParams startTime
  where
  dateParams :: Date -> String
  dateParams d = "&year=" <> (show <<< fromEnum <<< year) d <> "&month=" <> (show <<< fromEnum <<< month) d <> "&day=" <> (show <<< fromEnum <<< day) d
  timeParams :: Time -> String
  timeParams t = "&hour=" <> (show <<< fromEnum <<< hour) t <> "&minute=" <> (show <<< fromEnum <<< minute) t <> "&second=" <> (show <<< fromEnum <<< second) t

deserialize :: URI -> Either String Config
deserialize uri = case uri of
  (URI _ _ (Just (Query params)) _) -> buildConfig params
  _                                 -> Left "URI has no query parameters"
  where
  buildConfig :: List QueryParam -> Either String Config
  buildConfig ps = do event <- strParam ps "event"
                      prefix <- strParam ps "prefix"
                      startConfig <- buildWeeklyConfig ps <|> buildFixedConfig ps
                      pure $ Config { event, prefix, startConfig }
  buildWeeklyConfig :: List QueryParam -> Either String StartConfig
  buildWeeklyConfig ps = if strParam ps "type" == Right "weekly"
                         then do startDayOfWeek <- intParam ps "startDayOfWeek"
                                 startTime <- timeParam ps
                                 pure $ Weekly { startDayOfWeek, startTime }
                         else Left ("invalid parameters: " <> show ps)
  buildFixedConfig :: List QueryParam -> Either String StartConfig
  buildFixedConfig ps = if strParam ps "type" == Right "fixed"
                        then do startDate <- dateParam ps
                                startTime <- timeParam ps
                                pure $ Fixed { startDate, startTime }
                        else Left ("invalid parameters: " <> show ps)
  dateParam :: List QueryParam -> Either String Date
  dateParam ps = do y <- intParam ps "year"
                    m <- intParam ps "month"
                    d <- intParam ps "day"
                    canonicalDate <$> toEnumEither y <*> toEnumEither m <*> toEnumEither d
  timeParam :: List QueryParam -> Either String Time
  timeParam ps = do h <- intParam ps "hour"
                    m <- intParam ps "minute"
                    s <- intParam ps "second"
                    Time <$> toEnumEither h <*> toEnumEither m <*> toEnumEither s <*> toEnumEither 0
  toEnumEither :: forall a. BoundedEnum a => Int -> Either String a
  toEnumEither i = note ("out of bounds: " <> show i) (toEnum i)
  intParam :: List QueryParam -> String -> Either String Int
  intParam ps name = strParam ps name >>= (fromString >>> note ("param '" <> name <> "' is invalid. params: " <> show ps))
  strParam :: List QueryParam -> String -> Either String String
  strParam ps name = note ("param '" <> name <> "' not found. params:" <> show ps) $ (find (\t -> fst t == name) ps) >>= snd

type QueryParam = (Tuple String (Maybe String))

