module Test.Main where

import Prelude
import Test.Duration (durationTests)
import Test.StrongCheck (SC)

main :: forall eff. SC eff Unit
main = do
  durationTests
