# Returns a summary of counts by nucleotide.
nucleotide_count <- function(input) {
    if (grepl("[^GCTA]", input)) {
        stop("invalid nucleotide")
    }

    (counts <- input
        |> strsplit("") |> unlist()
        |> table() |> as.list())

    if (is.null(counts$G)) {
        counts <- c(counts, list(G = 0))
    }
    if (is.null(counts$C)) {
        counts <- c(counts, list(C = 0))
    }
    if (is.null(counts$T)) {
        counts <- c(counts, list(T = 0))
    }
    if (is.null(counts$A)) {
        counts <- c(counts, list(A = 0))
    }
    counts
}
