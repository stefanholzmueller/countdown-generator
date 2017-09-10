module Main where

import Prelude

import Component (Query(..), component)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Now (NOW)
import Halogen as H
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)


main :: forall eff. Eff (HA.HalogenEffects (now :: NOW | eff)) Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  io <- runUI component unit body
  H.liftAff $ io.query $ H.action Tick

--  CR.runProcess (hashChangeProducer CR.$$ hashChangeConsumer io.query)
  
--  H.liftEff $ setInterval 200 ?wat
--  where
--  sendTick :: HalogenIO -> Int
--  sendTick io = io.query $ H.action Tick
