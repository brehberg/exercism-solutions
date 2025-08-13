# Returns the Scrabble score for a given word.
scrabble_score <- function(input) {
    if (input == "") {
        return(0)
    }

    score_letter <- Vectorize(function(char) {
        if (char %in% c("A", "E", "I", "O", "U", "L", "N", "R", "S", "T")) {
            1
        } else if (char %in% c("D", "G")) {
            2
        } else if (char %in% c("B", "C", "M", "P")) {
            3
        } else if (char %in% c("F", "H", "V", "W", "Y")) {
            4
        } else if (char %in% c("K")) {
            5
        } else if (char %in% c("J", "X")) {
            8
        } else if (char %in% c("Q", "Z")) {
            10
        } else {
            0
        }
    })

    (input |> toupper()
        |> strsplit("") |> unlist()
        |> score_letter() |> sum())
}
