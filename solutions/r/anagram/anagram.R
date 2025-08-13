# Returns all candidates that are anagrams of, but not equal to, 'subject'.
anagram <- function(subject, candidates) {
    sorted <- function(input) {
        sort(strsplit(input, "")[[1]])
    }

    is_anagram <- function(word, base) {
        nchar(word) == nchar(base) &&
            word != base &&
            all(sorted(word) == sorted(base))
    }

    output <- c()
    for (word in candidates) {
        if (is_anagram(toupper(word), toupper(subject))) {
            output <- append(output, word)
        }
    }
    output
}
