proc isLeapYear*(year: int): bool =
  func isDivisibleBy(n: int): bool = year mod n == 0
  isDivisibleBy(4) and not isDivisibleBy(100) or isDivisibleBy(400)
