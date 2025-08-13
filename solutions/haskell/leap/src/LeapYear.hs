module LeapYear (isLeapYear) where

isDivisibleBy :: Integer -> Integer -> Bool
isDivisibleBy n year = mod year n == 0

isLeapYear :: Integer -> Bool
isLeapYear year =
  isDivisibleBy 4 year
    && not (isDivisibleBy 100 year)
    || isDivisibleBy 400 year
