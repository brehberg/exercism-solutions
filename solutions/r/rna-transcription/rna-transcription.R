# Transcribes a character list representing DNA nucleotides to RNA
to_rna <- function(dna) {
    if (grepl("[^GCTA]", dna)) {
        stop("invalid nucleotide")
    }

    dna_to_rna <- Vectorize(function(nucleotide) {
        switch(nucleotide,
            "G" = "C",
            "C" = "G",
            "T" = "A",
            "A" = "U",
        )
    })

    (dna |> strsplit("") |> unlist()
        |> dna_to_rna() |> paste(collapse = ""))
}
