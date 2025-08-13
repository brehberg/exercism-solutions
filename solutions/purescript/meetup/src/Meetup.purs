module Meetup
  ( meetup
  , Week(..)
  ) where

import Prelude
import Data.Date (Date, Month, Weekday, Year, canonicalDate, lastDayOfMonth, weekday)
import Data.Enum (fromEnum, toEnum)
import Data.Maybe (Maybe, fromJust)
import Partial.Unsafe (unsafePartial)

data Week
  = First
  | Second
  | Third
  | Fourth
  | Last
  | Teenth

firstDayOf :: Year -> Month -> Week -> Int
firstDayOf y m week = case week of
  First -> 1
  Second -> 8
  Third -> 15
  Fourth -> 22
  Teenth -> 13 
  Last -> (fromEnum $ lastDayOfMonth y m) - 6

meetup :: Year -> Month -> Week -> Weekday -> Maybe Date
meetup year month week requestedWeekday =
  canonicalDate year month <$> (toEnum day)
  where
    start = firstDayOf year month week
    guess = unsafePartial $ fromJust $ canonicalDate year month <$> toEnum start
    day = start + mod (fromEnum requestedWeekday - (fromEnum $ weekday guess)) 7
    