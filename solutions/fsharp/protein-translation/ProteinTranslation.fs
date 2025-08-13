module ProteinTranslation

let proteins (rna: string) : string list =
    let fromCodon (strand: string) : (string * string) option =
        match strand[0..2] with
        | "AUG" -> Some("Methionine", strand[3..])
        | "UUU"
        | "UUC" -> Some("Phenylalanine", strand[3..])
        | "UUA"
        | "UUG" -> Some("Leucine", strand[3..])
        | "UCU"
        | "UCC"
        | "UCA"
        | "UCG" -> Some("Serine", strand[3..])
        | "UAU"
        | "UAC" -> Some("Tyrosine", strand[3..])
        | "UGU"
        | "UGC" -> Some("Cysteine", strand[3..])
        | "UGG" -> Some("Tryptophan", strand[3..])
        | "UAA"
        | "UAG"
        | "UGA"
        | "" -> None
        | _ -> failwith "invalid codon"

    List.unfold fromCodon rna
