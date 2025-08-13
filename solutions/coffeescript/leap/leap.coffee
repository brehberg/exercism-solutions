class Leap
  @leapYear: (year) ->
    divisibleBy = (n) -> year % n == 0
    
    divisibleBy(4) && 
      !divisibleBy(100) || 
      divisibleBy(400)

module.exports = Leap
