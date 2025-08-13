# Bob returns a string that only ever answers one of five things.
#
# Bob is a lackadaisical teenager. He likes to think that he's very cool.
# And he definitely doesn't get excited about things. That wouldn't be cool.
bob <- function(input) {
    reply <- list(
        default = "Whatever.",
        question = "Sure.",
        yelling = "Whoa, chill out!",
        yellingQuestion = "Calm down, I know what I'm doing!",
        silence = "Fine. Be that way!"
    )

    input <- trimws(input)
    if (input == "") {
        return(reply$silence)
    }

    is_yelling <- input == toupper(input) && input != tolower(input)
    is_question <- substr(input, nchar(input), nchar(input)) == "?"

    if (is_yelling && is_question) {
        reply$yellingQuestion
    } else if (is_yelling) {
        reply$yelling
    } else if (is_question) {
        reply$question
    } else {
        reply$default
    }
}
