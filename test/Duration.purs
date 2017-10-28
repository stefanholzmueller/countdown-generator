module Test.Duration where

import Prelude

import Data.Time.Duration (Milliseconds(..), convertDuration)
import Data.Ord (abs)
import Test.StrongCheck (class Arbitrary, Result, SC, assertEq, quickCheck, (<?>))
import Test.StrongCheck.Gen (choose)
import Weekend (ItemizedDuration(..))


durationTests :: forall eff. SC eff Unit
durationTests = do
  quickCheck convertToItemizedDurationAndBack
  quickCheck itemizedDurationHasCorrectRanges

convertToItemizedDurationAndBack :: RandomMilliseconds -> Result
convertToItemizedDurationAndBack (RandomMilliseconds milliseconds) =
  assertEq milliseconds returnedMilliseconds
  where
  itemizedDuration :: ItemizedDuration
  itemizedDuration = convertDuration milliseconds
  returnedMilliseconds :: Milliseconds
  returnedMilliseconds = convertDuration itemizedDuration

itemizedDurationHasCorrectRanges :: RandomMilliseconds -> Result
itemizedDurationHasCorrectRanges (RandomMilliseconds milliseconds) =
  (((abs itemized.ms) < 1000.0) <?> ("incorrect ms: " <> show itemized.ms)) <>
  (((abs itemized.seconds) < 60) <?> ("incorrect seconds: " <> show itemized.seconds)) <>
  (((abs itemized.minutes) < 60) <?> ("incorrect minutes: " <> show itemized.minutes)) <>
  (((abs itemized.hours) < 24) <?> ("incorrect hours: " <> show itemized.hours))
  where
  itemizedDuration :: ItemizedDuration
  itemizedDuration = convertDuration milliseconds
  (Itemized itemized) = itemizedDuration

newtype RandomMilliseconds = RandomMilliseconds Milliseconds
instance arbMilliseconds :: Arbitrary RandomMilliseconds
  where
  arbitrary = do
    number <- choose (-9007199254740991.0) 9007199254740991.0
    pure (RandomMilliseconds $ Milliseconds number)
