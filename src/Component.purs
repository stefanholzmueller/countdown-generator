module Component where

import Prelude

import Control.Monad.Aff (Aff, delay)
import Control.Monad.Eff.Now (NOW)
import Data.Enum (fromEnum)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Time.Duration (Days(..), Hours(..), Milliseconds(..), Minutes(..), Seconds(..), toDuration)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Weekend (durationTillWeekend)
import Weekend as W

data Query a = Tick a

type State = { isWeekend :: Boolean, currentTime :: String, duration :: W.Duration }

component :: forall eff. H.Component HH.HTML Query Unit Void (Aff (now :: NOW | eff))
component =
  H.component
    { initialState: const initialState
    , render
    , eval
    , receiver: const Nothing
    }
  where

  dummyDuration :: W.Duration
  dummyDuration = { days: Days 0.0, hours: Hours 0.0, minutes: Minutes 0.0, seconds: Seconds 0.0 }

  initialState :: State
  initialState = { isWeekend: false, currentTime: "", duration: dummyDuration }

  render :: State -> H.ComponentHTML Query
  render state =
    if state.isWeekend then
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
                [ HH.text countdown ]
            ]
    where
    countdown = let d = show state.duration.days
                    h = show state.duration.hours
                    m = show state.duration.minutes
                    s = show state.duration.seconds
                in d <> "d " <> h <> "h " <> m <> "m " <> s <> "s"

  eval :: Query ~> H.ComponentDSL State Query Void (Aff (now :: NOW | eff))
  eval = case _ of
    Tick next -> do
      H.liftAff $ delay (Milliseconds 100.0)
      currentTime <- H.liftEff W.formattedCurrentTime
      isWeekend <- H.liftEff W.isWeekend
      durationTillWeekend <- H.liftEff W.durationTillWeekend
      let duration = fromMaybe dummyDuration durationTillWeekend
      H.put { isWeekend, currentTime, duration }
      eval (Tick next)
