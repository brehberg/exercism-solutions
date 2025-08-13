# Return difference between the two sums for a given number.
difference_of_squares <- function(natural_number) {
    square_of_sum(natural_number) - sum_of_squares(natural_number)
}

# Calculate sum of squares from 1 to a given end number.
sum_of_squares <- function(n) {
    (n + 3 * n^2 + 2 * n^3) / 6
}

# Calculate square of sum from 1 to a given end number.
square_of_sum <- function(n) {
    ((n + n^2) / 2)^2
}
