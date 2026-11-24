-- | In-game time. The paper is dated from the day it is read: round 1 is that day, and each later round moves the
-- | date on by the months a round stands for, which the engine reports with each round (`roundMonths`).
module Edition.Calendar (Date, issueDate, addMonths) where

import Prelude

-- | A calendar date: month 1–12, day 1–31.
type Date = { year :: Int, month :: Int, day :: Int }

-- | The date of the paper for a round, given the day it starts from and the months a round stands for. Round 1 (and
-- | anything before it) is the start.
issueDate :: Int -> Date -> Int -> Date
issueDate roundMonths start round = addMonths (roundMonths * max 0 (round - 1)) start

-- | Move a date on by whole months, keeping its day unless the month is shorter (31 January + 1 month is
-- | 28 or 29 February).
addMonths :: Int -> Date -> Date
addMonths n d =
  let
    index = d.year * 12 + (d.month - 1) + n
    year = index `div` 12
    month = index `mod` 12 + 1
  in
    { year, month, day: min d.day (daysIn year month) }

daysIn :: Int -> Int -> Int
daysIn year month =
  let
    leap = (year `mod` 4 == 0 && year `mod` 100 /= 0) || year `mod` 400 == 0
  in
    if month == 2 then (if leap then 29 else 28)
    else if month == 4 || month == 6 || month == 9 || month == 11 then 30
    else 31
