# Calculates the rows of a pascal triangle with given height
pascals_triangle <- function(n) {
    stopifnot(n >= 0)

    result <- list()
    while (n > 0) {
        if (length(result) == 0) {
            result[1] <- 1
        } else {
            previous <- unlist(tail(result, 1))
            next_row <- c(1, previous + c(previous[-1], 0))
            result <- append(result, list(next_row))
        }
        n <- n - 1
    }
    result
}
