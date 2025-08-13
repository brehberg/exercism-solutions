# Sieve of Eratosthenes algorithm for finding
# all prime numbers up to the given limit.
sieve <- function(limit) {
    if (limit < 2) {
        return(c())
    }
    if (limit == 2) {
        return(c(2))
    }

    # initial candidate list is all odd numbers starting from 3
    nums <- seq(3, limit, 2)
    index <- 1

    while (length(nums) > index) {
        n <- nums[index] # get next prime
        if (n^2 < limit) {
            # remove all odd multiples of next prime
            # starting from that prime squared
            multiples <- seq(n^2, limit, 2 * n)
            nums <- nums[!nums %in% multiples]
        }
        index <- index + 1
    }
    c(2, nums)
}
