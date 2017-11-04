module Component where

import Prelude

import Config (config)
import Control.Monad.Aff (Aff, delay)
import Control.Monad.Eff.Now (NOW)
import Countdown as C
import Data.DateTime (DateTime)
import Data.Either (either)
import Data.Formatter.DateTime (formatDateTime)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Time.Duration (Milliseconds(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Weekend as W

data Query a = Tick a

type State = { eventReached :: Boolean, currentTime :: String, duration :: Maybe String }

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
  initialState = { eventReached: false, currentTime: "", duration: Nothing }

  render :: State -> H.ComponentHTML Query
  render state =
    if state.eventReached then
        HH.div [ HP.class_ $ H.ClassName "rainbow" ]
            [ HH.h1 [ HP.class_ $ H.ClassName "time" ]
                [ HH.text ("It's " <> state.currentTime) ]
            , HH.h1 [ HP.class_ $ H.ClassName "weekend" ]
                [ HH.text "WEEKEND" ]
            ]
    else 
        HH.div_
            [ HH.h1 [ HP.class_ $ H.ClassName "time" ]
                [ HH.text ("It's " <> state.currentTime) ]
            , HH.h1 [ HP.class_ $ H.ClassName "countdown" ]
                [ HH.text "Weekend starts in"
                , HH.br_
                , HH.text $ fromMaybe "" state.duration
                ]
            ]

  eval :: Query ~> H.ComponentDSL State Query Void (Aff (now :: NOW | eff))
  eval = case _ of
    Tick next -> do
      H.liftAff $ delay (Milliseconds 100.0)
      currentLocalTime <- H.liftEff C.currentLocalTime
      let currentTime = formatCurrentTime currentLocalTime
      let eventReached = C.isEventReached config currentLocalTime
      let duration = W.durationTillWeekend currentLocalTime
      H.put { eventReached, currentTime, duration }
      eval (Tick next)


formatCurrentTime :: DateTime -> String
formatCurrentTime = formatDateTime "dddd, HH:mm" >>> either ("ERROR: " <> _) id
