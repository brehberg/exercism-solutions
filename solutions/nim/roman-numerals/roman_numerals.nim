import std/strutils

const numerals = block:
  const maxNumeral = 3999
  const literals = [
      (1000, "M", "", ""),
      (100, "C", "D", "M"),
      (10, "X", "L", "C"),
      (1, "I", "V", "X"),
  ]

  func toRomanNumeral(n: int): string = 
    var remaining = n

    for (value, unit, half, next) in literals:
      var digit = remaining div value
      case digit:      
        of 1, 2, 3: 
          result.add unit.repeat(digit)
        of 4: 
          result.add unit & half
        of 5, 6, 7, 8: 
          result.add half & unit.repeat(digit-5)
        of 9: 
          result.add unit & next        
        else:
          continue
      remaining = remaining mod value

  func generateNumerals: array[maxNumeral + 1, string] =
    for n in 1..maxNumeral:
      result[n] = toRomanNumeral(n)

  generateNumerals()

proc roman*(n: int): string =
  numerals[n]
