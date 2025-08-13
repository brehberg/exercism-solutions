# divisible returns if a given n is even divisible by d.
divisible <- function(n, d) {
    n %% d == 0
}

# raindrops returns a string that contains sounds corresponding to factors.
raindrops <- function(number) {
    sounds <- c(
        "Pling", "Plang", "PlingPlang", "Plong",
        "PlingPlong", "PlangPlong", "PlingPlangPlong"
    )

    # The rules of raindrops are that if a given number:
    # 	has 3 as a factor, add 'Pling' to the result.
    # 	has 5 as a factor, add 'Plang' to the result.
    # 	has 7 as a factor, add 'Plong' to the result.
    # 	does not have any of 3, 5, or 7 as a factor, result should be the number.
    index <- divisible(number, 3) +
        2 * divisible(number, 5) +
        4 * divisible(number, 7)

    if (index) {
        sounds[index]
    } else {
        paste0(number)
    }
}
