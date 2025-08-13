module Leap exposing (isLeapYear)


isLeapYear : Int -> Bool
isLeapYear year =
    let
        divisibleBy n =
            modBy n year == 0
    in
    divisibleBy 4 && not (divisibleBy 100) || divisibleBy 400
