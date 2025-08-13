# Generate an acronym from a string. "This is a string" => "TIAS"
acronym <- function(input) {
    str_replace_all <- \(x, p, r, ...) gsub(p, r, x, ...)

    (input |> str_replace_all("_", "")
        |> strsplit("[-[:blank:]]") |> unlist()
        |> substr(1, 1)
        |> paste(collapse = "") |> toupper())
}
