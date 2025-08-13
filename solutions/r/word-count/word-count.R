# Count the number of words in the input sentence.
word_count <- function(input) {
    punctuation <- "[^\\w'-]|_" # Not "words", apostrophe, and hyphen.

    # The only punctuation that does not separate words is the
    # apostrophe in contractions. Words are case insensitive.
    clean <- gsub(punctuation, " ", tolower(input), perl = TRUE)

    # Words can be separated by multiple punctuation or whitespace.
    words <- strsplit(trimws(clean), "\\s+")[[1]]

    # Trim words having either leading or trailing apostrophes.
    words <- gsub("^'|'$", "", words)

    # Return a list with the fequency count of each unique word.
    as.list(table(words))
}
