# Identify "saddle points" that are greater than or equal to every element
# in its row and less than or equal to every element in its column.
saddle_point <- function(input) {
    # determine max value for each row and min value for each column
    row_max <- apply(input, 1, max, -Inf)
    col_min <- apply(input, 2, min, Inf)

    # return matrix indicies where row max is equal to column min
    outer(row_max, col_min, FUN = "==") |>
        which(arr.ind = TRUE) |>
        as.data.frame()
}
