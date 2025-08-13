# Given a number, convert it into a roman numeral.
roman <- function(arabic) {
  stopifnot(arabic >= 1 && arabic <= 3999)

  literals <- c(
    M = 1000, CM = 900,
    D = 500, CD = 400,
    C = 100, XC = 90,
    L = 50, XL = 40,
    X = 10, IX = 9,
    V = 5, IV = 4,
    I = 1
  )

  roman <- c()
  for (label in names(literals)) {
    value <- literals[label]
    while (arabic >= value) {
      roman <- append(roman, label)
      arabic <- arabic - value
    }
  }
  paste(roman, collapse = "")
}
