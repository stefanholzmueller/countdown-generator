module Main where

import Prelude

import Component (Query(..), component)
import Control.Monad.Aff.Console (CONSOLE, log)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Now (NOW)
import DOM.HTML (window)
import DOM.HTML.Location (search)
import DOM.HTML.Window (location)
import Data.URI.Query (parser)
import Halogen as H
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Text.Parsing.StringParser (runParser)


main :: forall eff. Eff (HA.HalogenEffects (console :: CONSOLE, now :: NOW | eff)) Unit
main = HA.runHalogenAff do
  w <- H.liftEff window
  l <- H.liftEff $ location w
  s <- H.liftEff $ search l
  log s
  log $ show $ runParser parser s
  body <- HA.awaitBody
  io <- runUI component unit body
  H.liftAff $ io.query $ H.action Tick
