module Leap

let leapYear (year: int) : bool =
    let divisibleBy (n: int) : bool = year % n = 0
    (divisibleBy 4) && not (divisibleBy 100) || (divisibleBy 400)
