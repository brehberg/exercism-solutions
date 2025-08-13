module RnaTranscription exposing (toRNA)


dnaToRna : Char -> Char
dnaToRna nucleotide =
    case nucleotide of
        'G' ->
            'C'

        'C' ->
            'G'

        'T' ->
            'A'

        'A' ->
            'U'

        _ ->
            '?'


toRNA : String -> Result String String
toRNA dna =
    let
        rna =
            dna |> String.map dnaToRna
    in
    if rna |> String.contains "?" then
        Err "Invalid nucleotide detected."

    else
        Ok rna
