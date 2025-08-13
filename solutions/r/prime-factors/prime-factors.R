# Compute the prime factors for 'number'. The prime factors are
# prime numbers that when multiplied give the desired number.
# The prime factors of 'number' will be ordered lowest to highest.
prime_factors <- function(number) {
    if (number < 2) {
        return(c())
    }

    factors <- c()
    prime <- 2
    remaining <- number

    while (remaining / prime >= prime) {
        if (remaining %% prime == 0) {
            remaining <- remaining / prime
            factors <- c(factors, prime)
        } else {
            prime <- prime + 1
        }
    }
    c(factors, remaining)
}
