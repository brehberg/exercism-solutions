normalized_plaintext <- function(input) {
  tolower(gsub("\\W", "", input))
}

chunk_rows <- function(input) {
  chars <- unlist(strsplit(normalized_plaintext(input), ""))
  width <- ceiling(sqrt(length(chars)))
  # pad chars vector with spaces to produce even chunks
  length(chars) <- width * floor(sqrt(length(chars)))
  chars[is.na(chars)] <- " "
  # break vector into list of vectors of equal chunk size
  split(chars, ceiling(seq_along(chars) / width))
}

plaintext_segments <- function(input) {
  result <- c()
  for (chunk in chunk_rows(input)) {
    result <- append(result, trimws(paste(chunk, collapse = "")))
  }
  if (is.null(result)) "" else result
}

encoded <- function(input) {
  # use rbind to transform rows into matrix for output
  chunks <- do.call(rbind, chunk_rows(input))
  # concatenate all matrix values and remove whitespace
  gsub("\\s", "", paste(chunks, collapse = ""))
}

ciphertext <- function(input) {
  # create ouput matrix with row of spaces between chunks
  chunks <- do.call(rbind, c(chunk_rows(input), rep(" ")))
  # concatenate matrix values and remove final space only
  sub("\\s$", "", paste(chunks, collapse = ""))
}
