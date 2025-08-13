# An Armstrong number is a number that is the sum of its own
# digits each raised to the power of the number of digits.
is_armstrong_number <- function(n) {
    if (n == 0) {
        return(TRUE)
    }

    number <- n
    sum <- 0
    digits <- floor(log10(n)) + 1

    while (n > 0) {
        sum <- sum + (n %% 10)**digits
        n <- n %/% 10
    }

    (number == sum)
}
