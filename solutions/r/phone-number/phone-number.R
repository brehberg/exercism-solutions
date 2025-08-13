# parse_phone_number cleans up user-entered phone
# numbers so that they can be sent SMS messages.
parse_phone_number <- function(number_string) {
    # extract all digits from the given string
    m <- gregexpr("\\d", number_string)
    digits <- regmatches(number_string, m)[[1]]

    # all NANP-numbers share the same country code
    if (length(digits) == 11 && digits[1] == 1) {
        digits <- digits[-1]
    }

    # area and exchange codes only digits from 2 through 9
    if (length(digits) != 10 ||
        digits[1] == 0 || digits[1] == 1 ||
        digits[4] == 0 || digits[4] == 1) {
        return(NULL)
    }

    paste(digits, collapse = "")
}
