#  collatz_step_counter takes an integer and returns the number of steps
#  required to get the number to 1 when following the rules:
#    - if number is odd, multiply with 3 and add 1
#    - if number is even, divide by 2
collatz_step_counter <- Vectorize(function(num) {
    if (num < 1) {
        stop("invalid starting value, num must be a positive integer")
    }

    steps <- 0
    while (num > 1) {
        if (num %% 2 == 0) {
            num <- num / 2
        } else {
            num <- 3 * num + 1
        }
        steps <- steps + 1
    }
    steps
})
