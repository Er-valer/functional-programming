module HW1.T1
  ( Day (..)
  , afterDays
  , daysToParty
  , isWeekend
  , nextDay
  ) where

import Numeric.Natural (Natural)

data Day
  = Monday
  | Tuesday
  | Wednesday
  | Thursday
  | Friday
  | Saturday
  | Sunday
  deriving Show

nextDay :: Day -> Day
nextDay day =
  case day of
    Monday    -> Tuesday
    Tuesday   -> Wednesday
    Wednesday -> Thursday
    Thursday  -> Friday
    Friday    -> Saturday
    Saturday  -> Sunday
    Sunday    -> Monday

afterDays :: Natural -> Day -> Day
afterDays n day
  | n == 0    = day
  | otherwise = afterDays (n - 1) (nextDay day)

isWeekend :: Day -> Bool
isWeekend day = 
  case day of
    Saturday -> True
    Sunday   -> True
    _        -> False

daysToParty :: Day -> Natural
daysToParty day =
  case day of
    Friday -> 0
    _      -> daysToParty (nextDay day) + 1
