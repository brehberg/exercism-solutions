module Leap
  ( isLeapYear
  ) where

import Prelude

isLeapYear :: Int -> Boolean
isLeapYear year = 
  isDivisibleBy 4 && not isDivisibleBy 100 || isDivisibleBy 400
  where isDivisibleBy n = mod year n == 0

