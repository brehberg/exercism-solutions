# Determines if a word or sentence is a pangram.
# A pangram is a sentence using every letter of the alphabet at least once.
is_pangram <- function(input) {
    (charlist <- input
        |> tolower()
        |> strsplit("")
        |> unlist())

    all(letters %in% charlist)
}
