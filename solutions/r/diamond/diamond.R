diamond <- function(letter) {
    if (letter == "A") {
        return("A")
    }

    # find starting index of given letter and create middle row
    start <- index <- match(letter, LETTERS)
    center <- repeat_spaces(start * 2 - 3)
    output <- paste0(letter, center, letter)

    # add additional rows for previous letters until reaching "A"
    while (index > 2) {
        index <- index - 1
        letter <- LETTERS[index]
        padding <- repeat_spaces(start - index)
        center <- repeat_spaces(index * 2 - 3)
        next_row <- paste0(padding, letter, center, letter, padding)
        output <- enhance_output(next_row, output)
    }

    # add the first and last "A" rows to the final output
    padding <- repeat_spaces(start - 1)
    last_row <- paste0(padding, "A", padding)
    enhance_output(last_row, output)
}

repeat_spaces <- function(n) {
    paste(rep(" ", n), collapse = "")
}

enhance_output <- function(new_row, existing) {
    paste(new_row, existing, new_row, sep = "\n")
}
