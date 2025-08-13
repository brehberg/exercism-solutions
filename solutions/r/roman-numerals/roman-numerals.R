# raindrops returns a string that contains sounds corresponding to factors.
roman <- function(arabic) {
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
    while (arabic >= literals[label]) {
      roman <- append(roman, label)
      arabic <- arabic - literals[label]
    }
  }

  paste(roman, collapse = "")
}
