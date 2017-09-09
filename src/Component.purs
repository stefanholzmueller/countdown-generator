module Component where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Now (locale, now, nowDateTime)
import Control.Monad.Eff.Unsafe (unsafePerformEff)
import Data.DateTime (DateTime(..), adjust)
import Data.DateTime.Instant (toDateTime)
import Data.DateTime.Locale (LocalDateTime, LocalValue(..), Locale(..))
import Data.Formatter.DateTime (formatDateTime)
import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE

data Query a = ToggleState a

type State = { on :: Boolean }

component :: forall m. H.Component HH.HTML Query Unit Void m
component =
  H.component
    { initialState: const initialState
    , render
    , eval
    , receiver: const Nothing
    }
  where

  initialState :: State
  initialState = { on: false }

  offset (Locale _ min) = negate min

  adjusted = adjust (offset (unsafePerformEff locale)) (toDateTime (unsafePerformEff now))

  weekend :: LocalValue DateTime
  weekend = (unsafePerformEff nowDateTime)

  formatted = map (formatDateTime "YYYY MM DD HH mm ss") adjusted

  render :: State -> H.ComponentHTML Query
  render state =
    HH.div_
      [ HH.h1_
          [ HH.text (show formatted) ]
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

  eval :: Query ~> H.ComponentDSL State Query Void m
  eval = case _ of
    ToggleState next -> do
      H.modify (\state -> { on: not state.on })
      pure next
