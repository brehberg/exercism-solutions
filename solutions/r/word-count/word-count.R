library(stringr)

# Count the number of words in the input sentence.
word_count <- function(input) {
    punctuation <- "[^\\w'-]|_" # Not "words", apostrophe, and hyphen.

    words <- input |>
        # The only punctuation that does not separate words is the
        # apostrophe in contractions. Words are case insensitive.
        tolower() |>
        str_replace_all(punctuation, " ") |>
        # Words can be separated by multiple punctuation or whitespace.
        trimws() |>
        strsplit("\\s+")

    words[[1]] |>
        # Trim words having either leading or trailing apostrophes.
        str_replace_all("^'|'$", "") |>
        # Return a list with the fequency count of each unique word.
        table() |>
        as.list()
}
