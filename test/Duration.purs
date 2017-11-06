module Test.Duration where

import Prelude

import Data.Int (toNumber)
import Data.Ord (abs)
import Data.Time.Duration (Milliseconds(..))
import Duration (MultiUnitDuration, toMultiUnitDuration)
import Test.StrongCheck (class Arbitrary, Result, SC, assertEq, quickCheck, (<?>))
import Test.StrongCheck.Gen (choose)


durationTests :: forall eff. SC eff Unit
durationTests = do
  quickCheck convertToMultiUnitDurationAndBack
  quickCheck multiUnitDurationHasCorrectRanges

convertToMultiUnitDurationAndBack :: RandomMilliseconds -> Result
convertToMultiUnitDurationAndBack (RandomMilliseconds milliseconds) =
  assertEq milliseconds returnedMilliseconds
  where
  mud :: MultiUnitDuration
  mud = toMultiUnitDuration milliseconds
  returnedMilliseconds :: Milliseconds
  returnedMilliseconds = Milliseconds (toNumber mud.days * msInDay + toNumber mud.hours * msInHour + toNumber mud.minutes * msInMinute + toNumber mud.seconds * msInSecond + mud.ms)
  msInSecond = 1000.0
  msInMinute = 60.0 * msInSecond
  msInHour = 60.0 * msInMinute
  msInDay = 24.0 * msInHour

multiUnitDurationHasCorrectRanges :: RandomMilliseconds -> Result
multiUnitDurationHasCorrectRanges (RandomMilliseconds milliseconds) =
  (((abs mud.ms) < 1000.0) <?> ("incorrect ms: " <> show mud.ms)) <>
  (((abs mud.seconds) < 60) <?> ("incorrect seconds: " <> show mud.seconds)) <>
  (((abs mud.minutes) < 60) <?> ("incorrect minutes: " <> show mud.minutes)) <>
  (((abs mud.hours) < 24) <?> ("incorrect hours: " <> show mud.hours))
  where
  mud :: MultiUnitDuration
  mud = toMultiUnitDuration milliseconds

newtype RandomMilliseconds = RandomMilliseconds Milliseconds
instance arbMilliseconds :: Arbitrary RandomMilliseconds
  where
  arbitrary = do
    number <- choose (-9007199254740991.0) 9007199254740991.0
    pure (RandomMilliseconds $ Milliseconds number)
