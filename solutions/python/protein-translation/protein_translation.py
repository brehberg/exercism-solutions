def proteins(strand):
    """Translate RNA sequences into proteins.

    :param strand: string - the RNA strand
    :return: [string] - a list of proteins
    """

    STOP_CODON = "STOP"

    def from_codon(codon):
        match codon:
            case c if c in ["AUG"]:
                return "Methionine"
            case c if c in ["UUU", "UUC"]:
                return "Phenylalanine"
            case c if c in ["UUA", "UUG"]:
                return "Leucine"
            case c if c in ["UCU", "UCC", "UCA", "UCG"]:
                return "Serine"
            case c if c in ["UAU", "UAC"]:
                return "Tyrosine"
            case c if c in ["UGU", "UGC"]:
                return "Cysteine"
            case c if c in ["UGG"]:
                return "Tryptophan"
            case c if c in ["UAA", "UAG", "UGA"]:
                return STOP_CODON

    result = []
    for i in range(0, len(strand), 3):
        amino_acid = from_codon(strand[i : i + 3])
        if amino_acid == STOP_CODON:
            break
        result.append(amino_acid)
    return result
