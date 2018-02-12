module Main where

import Prelude

import Component (Query(..), component)
import Config as C
import Control.Monad.Aff.Console (CONSOLE, log)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Now (NOW)
import DOM.HTML (window)
import DOM.HTML.Location (href)
import DOM.HTML.Window (location)
import Data.Either (Either(..))
import Data.URI.URI (parser)
import Halogen as H
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Text.Parsing.StringParser (runParser)


main :: forall eff. Eff (HA.HalogenEffects (console :: CONSOLE, now :: NOW | eff)) Unit
main = HA.runHalogenAff do
  h <- H.liftEff $ window >>= location >>= href
  let config = loadConfig h
  log $ show $ config
  body <- HA.awaitBody
  io <- runUI component unit body
  H.liftAff $ io.query $ H.action Tick
  where
  loadConfig :: String -> Either String C.Config
  loadConfig location = case runParser parser location of
    (Left parseError) -> Left $ show parseError
    (Right uri) -> C.deserialize uri
