# Given a plaintext and amount to shift by, return a rotated string.
rotate <- function(text, key) {
    do_shift <- function(c, n, start, base = 26) {
        start + (c + n - start) %% base
    }

    shift <- Vectorize(function(code) {
        if (intToUtf8(code) %in% letters) {
            do_shift(code, key, utf8ToInt("a"))
        } else if (intToUtf8(code) %in% LETTERS) {
            do_shift(code, key, utf8ToInt("A"))
        } else {
            code
        }
    })

    (text |> utf8ToInt() |> shift() |> intToUtf8())
}
