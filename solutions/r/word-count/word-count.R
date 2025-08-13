# Count the number of words in the input sentence.
# Words are compared case-insensitively.
word_count <- function(input) {
    # The only punctuation that does not separate words is the
    # apostrophe in contractions. Words are case insensitive.
    punctuation <- "[^\\w\'-]|_"
    clean <- gsub(punctuation, " ", tolower(input), perl = TRUE)

    # Words can be separated by multiple punctuation or whitespace.
    words <- strsplit(trimws(clean), "\\s+")[[1]]

    # Trim words with either leading or trailing apostrophes.
    quotes <- "^\'|\'$"
    words <- gsub(quotes, "", words)

    # return a list with the fequency count of each unique word
    as.list(table(words))
}
