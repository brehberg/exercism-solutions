# Finds the largest product for a specified number of
# consecutive numbers in a given string of numbers.
largest_series_product <- function(digits, span) {
    stopifnot(span >= 0)
    if (span == 0) {
        return(1)
    }

    (digits <- digits
        |> strsplit("") |> unlist()
        |> as.numeric() |> suppressWarnings())

    stopifnot(span <= length(digits))

    largest <- 0
    for (i in seq(digits)) {
        size <- i + span - 1
        if (size <= length(digits)) {
            largest <- max(largest, prod(digits[i:size]))
        }
    }
    largest
}
