module Component where

import Prelude

import Control.Monad.Aff (Aff, delay)
import Control.Monad.Eff.Now (NOW)
import Data.Maybe (Maybe(..))
import Data.Time.Duration (Milliseconds(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Weekend as W

data Query a = Tick a

type State = { isWeekend :: Boolean, currentTime :: String }

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
  initialState = { isWeekend: false, currentTime: "" }

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
                [ HH.text "countdown" ]
            ]

  eval :: Query ~> H.ComponentDSL State Query Void (Aff (now :: NOW | eff))
  eval = case _ of
    Tick next -> do
      H.liftAff $ delay (Milliseconds 100.0)
      currentTime <- H.liftEff W.formattedCurrentTime
      isWeekend <- H.liftEff W.isWeekend
      H.put { isWeekend, currentTime }
      eval (Tick next)
