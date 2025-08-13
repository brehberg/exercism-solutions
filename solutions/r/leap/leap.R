# divisible returns if a given n is even divisible by d.
divisible <- function(n, d) {
    n %% d == 0
}

# leap returns if a given year is a leap year.
leap <- function(year) {
    # Occurs on every year that is evenly divisible by 4
    # except every year that is evenly divisible by 100
    # unless the year is also evenly divisible by 400.
    if (divisible(year, 100)) {
        divisible(year, 400)
    } else {
        divisible(year, 4)
    }
}
