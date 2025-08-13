# Determines if a word or sentence is an isogram
is_isogram <- function(word) {
    (clean <- gsub("[-[:blank:]]", "", word)
        |> tolower()
        |> strsplit("")
        |> unlist())

    all(table(clean) == 1)
}
