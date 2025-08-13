# leap returns if a given year is a leap year.
leap <- function(year) {
    # divisible returns if a year is evenly divisible by d.
    divisible <- function(d) {
        year %% d == 0
    }

    # Occurs on every year that is evenly divisible by 4
    # except every year that is evenly divisible by 100
    # unless the year is also evenly divisible by 400.
    divisible(400) || (divisible(4) && !divisible(100))
}
