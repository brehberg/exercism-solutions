import gleam/list

pub fn proteins(rna: String) -> Result(List(String), Nil) {
  from_codons(rna, [])
}

fn from_codons(rna: String, acc: List(String)) -> Result(List(String), Nil) {
  case rna {
    "AUG" <> rest -> from_codons(rest, ["Methionine", ..acc])
    "UUU" <> rest -> from_codons(rest, ["Phenylalanine", ..acc])
    "UUC" <> rest -> from_codons(rest, ["Phenylalanine", ..acc])
    "UUA" <> rest -> from_codons(rest, ["Leucine", ..acc])
    "UUG" <> rest -> from_codons(rest, ["Leucine", ..acc])
    "UCU" <> rest -> from_codons(rest, ["Serine", ..acc])
    "UCC" <> rest -> from_codons(rest, ["Serine", ..acc])
    "UCA" <> rest -> from_codons(rest, ["Serine", ..acc])
    "UCG" <> rest -> from_codons(rest, ["Serine", ..acc])
    "UAU" <> rest -> from_codons(rest, ["Tyrosine", ..acc])
    "UAC" <> rest -> from_codons(rest, ["Tyrosine", ..acc])
    "UGU" <> rest -> from_codons(rest, ["Cysteine", ..acc])
    "UGC" <> rest -> from_codons(rest, ["Cysteine", ..acc])
    "UGG" <> rest -> from_codons(rest, ["Tryptophan", ..acc])
    "" | "UAA" <> _ | "UAG" <> _ | "UGA" <> _ -> Ok(list.reverse(acc))
    _ -> Error(Nil)
  }
}
