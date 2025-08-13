# Count the number of words in the input sentence.
word_count <- function(input) {
  punctuation <- "[^\\w'-]|_|^'|'$" # Not "words", apostrophe, or hyphen.
  str_replace_all <- \(x, p, r, ...) gsub(p, r, x, perl = TRUE, ...)

  # The only punctuation that does not separate words is the
  # apostrophe in contractions. Words are case insensitive.
  (input |> tolower()
    |> str_replace_all(punctuation, " ")
    |> trimws()
    # Words can be separated by multiple punctuation or whitespace.
    |> strsplit("\\s+")
    |> unlist()
    # Trim words having either leading or trailing apostrophes.
    |> str_replace_all("^'|'$", "")
    # Return a list with the fequency count of each unique word.
    |> table()
    |> as.list())
}
