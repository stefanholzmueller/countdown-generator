module Component where

import Prelude

import Control.Monad.Aff (Aff)
import Control.Monad.Eff.Now (NOW)
import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Weekend as W

data Query a = ToggleState a

type State = { on :: Boolean, currentTime :: String }

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
  initialState = { on: false, currentTime: "" }

  render :: State -> H.ComponentHTML Query
  render state =
    HH.div_
      [ HH.h1_
          [ HH.text ("It's " <> state.currentTime) ]
      , HH.p_
          [ HH.text "Why not toggle this button:" ]
      , HH.button
          [ HE.onClick (HE.input_ ToggleState) ]
          [ HH.text
              if not state.on
              then "Don't push me"
              else "I said don't push me!"
          ]
      ]

  eval :: Query ~> H.ComponentDSL State Query Void (Aff (now :: NOW | eff))
  eval = case _ of
    ToggleState next -> do
      currentTime <- H.liftEff W.formattedCurrentTime
      H.modify (\state -> state { currentTime = currentTime })
      pure next
