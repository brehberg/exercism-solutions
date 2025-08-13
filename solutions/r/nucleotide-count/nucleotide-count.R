# Returns a summary of counts by nucleotide.
nucleotide_count <- function(input) {
    if (grepl("[^GCTA]", input)) {
        stop("invalid nucleotide")
    }

    (input |> strsplit("") |> unlist()
        |> factor(levels = c("G", "C", "T", "A"))
        |> table() |> as.list())
}
