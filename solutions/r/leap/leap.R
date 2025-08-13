# leap returns if a given year is a leap year.
leap <- function(year) {
  # is_divisible returns if a year is evenly divisible by n.
  divisible_by <- function(n) {
    year %% n == 0
  }

  # Occurs on every year that is evenly divisible by 4
  # except every year that is evenly divisible by 100
  # unless the year is also evenly divisible by 400.
  (divisible_by(4) && !divisible_by(100)) || divisible_by(400)
}
