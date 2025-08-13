# Adds up all numbers from 1 to a given end number that are multiples
# of the factors provided.
sum_of_multiples <- function(factors, limit) {
    numbers <- c(0)
    for (factor in factors[factors <= limit]) {
        numbers <- c(numbers, seq(factor, limit - 1, factor))
    }
    sum(unique(numbers))
}
