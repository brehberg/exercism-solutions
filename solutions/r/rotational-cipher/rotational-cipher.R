# Given a plaintext and amount to shift by, return a rotated string.
rotate <- function(text, key) {
    do_shift <- function(c, n, values) {
        offset <- utf8ToInt(c) + n - utf8ToInt(values[1])
        values[1 + offset %% length(values)]
    }

    shift <- Vectorize(function(char) {
        if (char %in% letters) {
            do_shift(char, key, letters)
        } else if (char %in% LETTERS) {
            do_shift(char, key, LETTERS)
        } else {
            char
        }
    })

    (text |> strsplit("") |> unlist() |> shift() |> paste(collapse = ""))
}
