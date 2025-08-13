# Compute the prime factors for 'number'. The prime factors are
# prime numbers that when multiplied give the desired number.
# The prime factors of 'number' will be ordered lowest to highest.
prime_factors <- function(number) {
    if (number < 2) {
        return(c())
    }

    primes <- c()
    prime <- 2
    n <- number

    while (n / prime >= prime) {
        if (n %% prime == 0) {
            n <- n / prime
            primes <- c(prime, primes)
        } else {
            prime <- prime + 1
        }
    }
    rev(c(n, primes))
}
