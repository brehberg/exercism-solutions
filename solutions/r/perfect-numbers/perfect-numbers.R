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
    if (n == 1) {
        return("deficient")
    }
    possible <- 1:(n - 1)
    aliquot_sum <- sum(possible[n %% possible == 0])

    ifelse(aliquot_sum > n, "abundant",
        ifelse(aliquot_sum < n, "deficient", "perfect")
    )
}
