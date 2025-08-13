# Returns all candidates that are anagrams of, but not equal to, 'subject'.
anagram <- function(subject, candidates) {
    sorted <- function(input) {
        sort(strsplit(input, "")[[1]])
    }

    base <- toupper(subject)
    is_anagram <- sapply(candidates, function(word) {
        word <- toupper(word)
        nchar(word) == nchar(base) &&
            word != base &&
            all(sorted(word) == sorted(base))
    })

    if (any(is_anagram)) {
        candidates[is_anagram]
    } else {
        c()
    }
}
