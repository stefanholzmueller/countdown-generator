module Component where

import Prelude

import Config (Config(..), StartConfig(..))
import Control.Monad.Aff (Aff, delay)
import Control.Monad.Eff.Now (NOW)
import Countdown as C
import Data.DateTime (DateTime)
import Data.Either (Either(..), either)
import Data.Formatter.DateTime (formatDateTime)
import Data.Maybe (Maybe(..))
import Data.Time.Duration (Milliseconds(..))
import Duration as D
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP


data Query a = Tick a

type State = { configOrError :: Either String Config, currentTime :: String, countdownResult :: C.CountdownResult }

mainComponent :: forall eff. Either String Config -> H.Component HH.HTML Query Unit Void (Aff (now :: NOW | eff))
mainComponent configOrError =
  H.component
    { initialState: const initialState
    , render
    , eval
    , receiver: const Nothing
    }
  where

  initialState :: State
  initialState = { configOrError, currentTime: "loading...", countdownResult: Nothing }

  render :: State -> H.ComponentHTML Query
  render state = case state.configOrError of 
    (Left error) -> HH.p_ [ HH.text ("ERROR: " <> error) ]
    (Right (Config { event, prefix, startConfig })) -> case state.countdownResult of
        Nothing ->
            HH.div [ HP.class_ $ H.ClassName "rainbow" ]
                [ HH.h1 [ HP.class_ $ H.ClassName "time" ]
                    [ HH.text ("It's " <> state.currentTime) ]
                , HH.h1 [ HP.class_ $ H.ClassName "weekend" ]
                    [ HH.text event ]
                ]
        (Just components) ->
            HH.div_
                [ HH.h1 [ HP.class_ $ H.ClassName "time" ]
                    [ HH.text ("It's " <> state.currentTime) ]
                , HH.h1 [ HP.class_ $ H.ClassName "countdown" ]
                    [ HH.text prefix
                    , HH.br_
                    , HH.text $ formatDuration components
                    ]
                ]

  eval :: Query ~> H.ComponentDSL State Query Void (Aff (now :: NOW | eff))
  eval = case _ of
    Tick next -> either (const (pure next)) (runWithConfig next) configOrError
      where
      runWithConfig next (Config { event, prefix, startConfig }) = do
        H.liftAff $ delay (Milliseconds 100.0)
        currentLocalTime <- H.liftEff C.currentLocalTime
        let currentTime = case startConfig of
                            (Weekly _) -> formatCurrentTime "dddd, HH:mm" currentLocalTime
                            (Fixed _) -> formatCurrentTime "dddd, MMMM D" currentLocalTime
        let countdownResult = C.countdown startConfig currentLocalTime
        H.put { currentTime, countdownResult, configOrError }
        eval (Tick next)


formatCurrentTime :: String -> DateTime -> String
formatCurrentTime format = formatDateTime format >>> either ("ERROR: " <> _) id

formatDuration :: D.DurationComponents -> String
formatDuration components = dd <> hh <> ":" <> mm <> ":" <> ss
  where
  formatTime t = if t < 10 then "0" <> show t else show t
  dd = case components.days of
        0 -> ""
        1 -> "1 day and "
        d -> show d <> " days and "
  hh = formatTime components.hours
  mm = formatTime components.minutes
  ss = formatTime components.seconds
