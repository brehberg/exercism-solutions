const sounds = block:
  const factors = [
    (3, "Pling"),
    (5, "Plang"),
    (7, "Plong"),
  ]

  func calculateLength(a: openArray[(int, string)]): int =
    ## determine the product of integers in array of factors
    result = 1
    for item in a:
      result *= item[0]

  func generateLookup: array[calculateLength(factors), string] =
    ## setup raindrop sounds corresponding to factor combinations
    for n, s in result.mpairs():
      for (val, sound) in factors:
        if n mod val == 0:
          s.add sound

  generateLookup()

proc convert*(num: int): string =
  result = sounds[num mod sounds.len]
  if result == "":
    result = $num
