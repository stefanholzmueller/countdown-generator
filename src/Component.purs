module Component where

import Prelude

import Config (Config)
import Control.Monad.Aff (Aff, delay)
import Control.Monad.Eff.Now (NOW)
import Countdown as C
import Data.DateTime (DateTime)
import Data.Either (Either, either)
import Data.Formatter.DateTime (formatDateTime)
import Data.Maybe (Maybe(..))
import Data.Time.Duration (Milliseconds(..))
import Duration as D
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP


data Query a = Tick a

type State = { currentTime :: String, countdownResult :: C.CountdownResult }

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
  initialState = { currentTime: "Loading...", countdownResult: Nothing }

  render :: State -> H.ComponentHTML Query
  render state = case state.countdownResult of
    Nothing ->
        HH.div [ HP.class_ $ H.ClassName "rainbow" ]
            [ HH.h1 [ HP.class_ $ H.ClassName "time" ]
                [ HH.text ("It's " <> state.currentTime) ]
            , HH.h1 [ HP.class_ $ H.ClassName "weekend" ]
                [ HH.text "WEEKEND" ]
            ]
    (Just components) ->
        HH.div_
            [ HH.h1 [ HP.class_ $ H.ClassName "time" ]
                [ HH.text ("It's " <> state.currentTime) ]
            , HH.h1 [ HP.class_ $ H.ClassName "countdown" ]
                [ HH.text "Weekend starts in"
                , HH.br_
                , HH.text $ formatDuration components
                ]
            ]

  eval :: Query ~> H.ComponentDSL State Query Void (Aff (now :: NOW | eff))
  eval = case _ of
    Tick next -> either (const (pure next)) (runWithConfig next) configOrError
      where
      runWithConfig next config = do
        H.liftAff $ delay (Milliseconds 100.0)
        currentLocalTime <- H.liftEff C.currentLocalTime
        let currentTime = formatCurrentTime currentLocalTime
        let countdownResult = C.countdown config currentLocalTime
        H.put { currentTime, countdownResult }
        eval (Tick next)


formatCurrentTime :: DateTime -> String
formatCurrentTime = formatDateTime "dddd, HH:mm" >>> either ("ERROR: " <> _) id

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
