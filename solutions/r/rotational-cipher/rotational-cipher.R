# Given a plaintext and amount to shift by, return a rotated string.
rotate <- function(text, key) {
    ascii <- list(
        a = utf8ToInt("a"),
        z = utf8ToInt("z"),
        A = utf8ToInt("A"),
        Z = utf8ToInt("Z")
    )

    do_shift <- function(c, n, start, base = 26) {
        start + (c + n - start) %% base
    }

    shift <- Vectorize(function(code) {
        if (intToUtf8(code) %in% letters) {
            do_shift(code, key, ascii$a)
        } else if (intToUtf8(code) %in% LETTERS) {
            do_shift(code, key, ascii$A)
        } else {
            code
        }
    })

    (text |> utf8ToInt() |> shift() |> intToUtf8())
}
