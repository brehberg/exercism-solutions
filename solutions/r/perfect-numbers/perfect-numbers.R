# Determine the aliquot sum of the given `number`, by summing all the factors
# of `number`, aside from `number` itself.

# Based on this sum, classify the number as:
#   perfect if the aliquot sum is equal to `number`
#   abundant if the aliquot sum is greater than `number`
#   deficient if the aliquot sum is less than `number`
number_type <- function(n) {
    if (n < 1) {
        stop("Classification is only possible for natural numbers.")
    }
    factors <- c()
    possible <- 1
    done <- n

    while (possible < done - 1) {
        if (n %% possible == 0) {
            done <- n / possible
            if (possible == done || done == n) {
                factors <- append(factors, possible)
            } else {
                factors <- append(factors, c(possible, done))
            }
        }
        possible <- possible + 1
    }
    aliquot_sum <- sum(factors)

    if (aliquot_sum > n) {
        "abundant"
    } else if (aliquot_sum < n) {
        "deficient"
    } else {
        "perfect"
    }
}
