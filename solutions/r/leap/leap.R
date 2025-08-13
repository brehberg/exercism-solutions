# leap returns if a given year is a leap year.

# Occurs on every year that is evenly divisible by 4
# except every year that is evenly divisible by 100
# unless the year is also evenly divisible by 400.
leap <- function(year) {
    if(year %% 100 == 0) {
        year %% 400 == 0
    } else {
        year %% 4 == 0
    }
}
