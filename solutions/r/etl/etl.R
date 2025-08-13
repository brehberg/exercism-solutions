# Transforms an old Scrabble score system to a new one.
etl <- function(input) {
    output <- list()
    for (i in seq(input)) {
        value <- as.numeric(names(input)[[i]])
        for (letter in input[[i]]) {
            output <- c(output, setNames(list(value), tolower(letter)))
        }
    }
    output[order(names(output))]
}
