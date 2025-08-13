# Transforms an old Scrabble score system to a new one.
etl <- function(input) {
    output <- list()
    for (key in names(input)) {
        output[tolower(input[[key]])] <- as.numeric(key)
    }
    output[order(names(output))]
}
