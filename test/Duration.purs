module Test.Duration where

import Prelude
import Data.Time.Duration (Milliseconds(..), convertDuration)
--import Debug.Trace (spy)
import Test.StrongCheck (class Arbitrary, Result, SC, assertEq, quickCheck)
import Test.StrongCheck.Gen (choose)
import Weekend (ItemizedDuration)


durationTests :: forall eff. SC eff Unit
durationTests = do
  quickCheck convertToItemizedDurationAndBack

convertToItemizedDurationAndBack :: RandomMilliseconds -> Result
convertToItemizedDurationAndBack (RandomMilliseconds milliseconds) =
  assertEq milliseconds returnedMilliseconds
  where
  itemizedDuration :: ItemizedDuration
  itemizedDuration = convertDuration milliseconds
  returnedMilliseconds :: Milliseconds
  returnedMilliseconds = convertDuration itemizedDuration

newtype RandomMilliseconds = RandomMilliseconds Milliseconds
instance arbMilliseconds :: Arbitrary RandomMilliseconds
  where
  arbitrary = do
    number <- choose (-9007199254740991.0) 9007199254740991.0
    pure (RandomMilliseconds $ Milliseconds number)

