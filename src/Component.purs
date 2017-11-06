module Component where

import Prelude

import Config (config)
import Control.Monad.Aff (Aff, delay)
import Control.Monad.Eff.Now (NOW)
import Countdown as C
import Data.DateTime (DateTime)
import Data.Either (either)
import Data.Formatter.DateTime (formatDateTime)
import Data.Maybe (Maybe(..))
import Data.Time.Duration (Milliseconds(..))
import Duration as D
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP


data Query a = Tick a

type State = { currentTime :: String, countdownResult :: C.CountdownResult }

component :: forall eff. H.Component HH.HTML Query Unit Void (Aff (now :: NOW | eff))
component =
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
    (Just multiUnitDuration) ->
        HH.div_
            [ HH.h1 [ HP.class_ $ H.ClassName "time" ]
                [ HH.text ("It's " <> state.currentTime) ]
            , HH.h1 [ HP.class_ $ H.ClassName "countdown" ]
                [ HH.text "Weekend starts in"
                , HH.br_
                , HH.text $ formatMultiUnitDuration multiUnitDuration
                ]
            ]

  eval :: Query ~> H.ComponentDSL State Query Void (Aff (now :: NOW | eff))
  eval = case _ of
    Tick next -> do
      H.liftAff $ delay (Milliseconds 100.0)
      currentLocalTime <- H.liftEff C.currentLocalTime
      let currentTime = formatCurrentTime currentLocalTime
      let countdownResult = C.countdown config currentLocalTime
      H.put { currentTime, countdownResult }
      eval (Tick next)


formatCurrentTime :: DateTime -> String
formatCurrentTime = formatDateTime "dddd, HH:mm" >>> either ("ERROR: " <> _) id

formatMultiUnitDuration :: D.MultiUnitDuration -> String
formatMultiUnitDuration duration = dd <> hh <> ":" <> mm <> ":" <> ss
  where
  formatTime t = if t < 10 then "0" <> show t else show t
  dd = case duration.days of
        0 -> ""
        1 -> "1 day and "
        d -> show d <> " days and "
  hh = formatTime duration.hours
  mm = formatTime duration.minutes
  ss = formatTime duration.seconds
