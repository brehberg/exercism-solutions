score <- function(x, y) {
    dist <- sqrt(x * x + y * y)
    ifelse(dist <= 1, 10,
        ifelse(dist <= 5, 5,
            ifelse(dist <= 10, 1, 0)
        )
    )
}
