module RnaTranscription exposing (toRNA)


dnaToRna : Char -> Result String Char
dnaToRna nucleotide =
    case nucleotide of
        'G' ->
            Ok 'C'

        'C' ->
            Ok 'G'

        'T' ->
            Ok 'A'

        'A' ->
            Ok 'U'

        _ ->
            Err "Invalid nucleotide detected."


toRNA : String -> Result String String
toRNA =
    String.foldr
        (\c ->
            Result.map2
                String.cons
                (dnaToRna c)
        )
        (Ok "")
