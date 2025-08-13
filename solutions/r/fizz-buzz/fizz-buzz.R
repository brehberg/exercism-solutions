# Replace numbers from 1 to n with the following rules:
#   When the number is a multiple of 3, print "Fizz"
#   When the number is a multiple of 5, print "Buzz"
#   When the number is both a multiple of 3 and 5, print "Fizz Buzz"
#   When the number is not a multiple of 3 or 5, print the number
fizz_buzz <- function(n) {
    terms <- c("Fizz", "Buzz", "Fizz Buzz")

    is_divisible <- function(n, d) {
        n %% d == 0
    }

    replace <- Vectorize(function(n) {
        index <- is_divisible(n, 3) + 2 * is_divisible(n, 5)
        if (index) {
            terms[index]
        } else {
            as.character(n)
        }
    })

    (1:n |> seq() |> replace())
}
