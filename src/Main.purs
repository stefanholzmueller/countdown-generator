module Main where

import Prelude

import Component (component)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Now (NOW)
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)

main :: forall eff. Eff (HA.HalogenEffects (now :: NOW | eff)) Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI component unit body
