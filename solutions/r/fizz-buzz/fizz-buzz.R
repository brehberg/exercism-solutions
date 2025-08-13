# Print numbers from 1 to n with the following rules:
#   When the number is a multiple of 3, print "Fizz"
#   When the number is a multiple of 5, print "Buzz"
#   When the number is both a multiple of 3 and 5, print "Fizz Buzz"
#   When the number is not a multiple of 3 or 5, print the number
fizz_buzz <- function(n) {
    replace <- Vectorize(function(number) {
        is_divisible_by <- function(d) {
            number %% d == 0
        }
        terms <- c(as.character(number), "Fizz", "Buzz", "Fizz Buzz")
        terms[1 + is_divisible_by(3) + 2 * is_divisible_by(5)]
    })
    replace(seq(1:n))
}
