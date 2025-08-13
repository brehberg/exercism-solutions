import std/strutils

const numerals = block:
  const maxNumeral = 3999
  const literals = [
      ("M", "", ""),
      ("C", "D", "M"),
      ("X", "L", "C"),
      ("I", "V", "X"),
  ]

  func toRomanNumeral(n: int): string = 
    var remaining = n
    var value = 1000

    for (unit, half, next) in literals:
      var digit = remaining div value
      case digit:
        of 0, 1, 2, 3: 
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
      value = int(value / 10)

  func generateNumerals: array[maxNumeral + 1, string] =
    for n in 1..maxNumeral:
      result[n] = toRomanNumeral(n)

  generateNumerals()

proc roman*(n: int): string =
  numerals[n]
