# Returns true if the given number is valid via the luhn formula.
is_valid <- function(input) {
    (id <- gsub(" ", "", input)
        |> strsplit("") |> unlist()
        |> as.numeric() |> suppressWarnings())

    if (length(id) < 2 || any(is.na(id))) {
        return(FALSE)
    }

    sum <- 0
    for (i in seq(id)) {
        n <- id[length(id) - i + 1]

        if (i %% 2 != 0) {
            sum <- sum + n
        } else if (n < 5) {
            sum <- sum + n * 2
        } else {
            sum <- sum + n * 2 - 9
        }
    }

    (sum %% 10 == 0)
}
