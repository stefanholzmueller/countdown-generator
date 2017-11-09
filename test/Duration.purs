module Test.Duration where

import Prelude

import Data.Int (toNumber)
import Data.Ord (abs)
import Data.Time.Duration (Milliseconds(..))
import Duration (DurationComponents, durationComponents)
import Test.StrongCheck (class Arbitrary, Result, SC, assertEq, quickCheck, (<?>))
import Test.StrongCheck.Gen (choose)


durationTests :: forall eff. SC eff Unit
durationTests = do
  quickCheck convertToDurationComponentsAndBack
  quickCheck durationComponentsHaveCorrectRanges

convertToDurationComponentsAndBack :: RandomMilliseconds -> Result
convertToDurationComponentsAndBack (RandomMilliseconds milliseconds) =
  assertEq milliseconds returnedMilliseconds
  where
  components :: DurationComponents
  components = durationComponents milliseconds
  returnedMilliseconds :: Milliseconds
  returnedMilliseconds = Milliseconds (toNumber components.days * msInDay + toNumber components.hours * msInHour + toNumber components.minutes * msInMinute + toNumber components.seconds * msInSecond + components.ms)
  msInSecond = 1000.0
  msInMinute = 60.0 * msInSecond
  msInHour = 60.0 * msInMinute
  msInDay = 24.0 * msInHour

durationComponentsHaveCorrectRanges :: RandomMilliseconds -> Result
durationComponentsHaveCorrectRanges (RandomMilliseconds milliseconds) =
  (((abs components.ms) < 1000.0) <?> ("incorrect ms: " <> show components.ms)) <>
  (((abs components.seconds) < 60) <?> ("incorrect seconds: " <> show components.seconds)) <>
  (((abs components.minutes) < 60) <?> ("incorrect minutes: " <> show components.minutes)) <>
  (((abs components.hours) < 24) <?> ("incorrect hours: " <> show components.hours))
  where
  components :: DurationComponents
  components = durationComponents milliseconds

newtype RandomMilliseconds = RandomMilliseconds Milliseconds
instance arbMilliseconds :: Arbitrary RandomMilliseconds
  where
  arbitrary = do
    number <- choose (-9007199254740991.0) 9007199254740991.0
    pure (RandomMilliseconds $ Milliseconds number)
