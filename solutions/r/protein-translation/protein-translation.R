translate <- function(bases) {
  if (nchar(bases) == 0) {
    return(c())
  }

  group_chunks <- function(strand) {
    # group_chunks returns a vector of strings with max length 3
    regmatches(strand, gregexpr(".{1,3}", strand)) |> unlist()
  }

  from_codon <- function(codon) {
    switch(codon,
      AUG = "Methionine",
      UUU = ,
      UUC = "Phenylalanine",
      UUA = ,
      UUG = "Leucine",
      UCU = ,
      UCC = ,
      UCA = ,
      UCG = "Serine",
      UAU = ,
      UAC = "Tyrosine",
      UGU = ,
      UGC = "Cysteine",
      UGG = "Tryptophan",
      UAA = ,
      UAG = ,
      UGA = "STOP"
    )
  }

  result <- c()
  for (chunk in group_chunks(bases)) {
    amino_acid <- from_codon(chunk)
    if (amino_acid == "STOP") break
    result <- c(result, amino_acid)
  }
  result
}
