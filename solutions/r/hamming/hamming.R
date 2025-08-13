# tokenize returns a vector with all of the characters from a string.
tokenize <- function(string) {
    strsplit(string, "")[[1]]
}

# This function takes two strings and calculates the hamming distance.
hamming <- function(strand1, strand2) {
    if (nchar(strand1) != nchar(strand2)) {
        stop("different lengths")
    }
    sum(tokenize(strand1) != tokenize(strand2))
}
