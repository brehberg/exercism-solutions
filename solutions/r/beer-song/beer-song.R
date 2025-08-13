# Get the entire beer song for a given range of numbers of bottles.
lyrics <- function(first, last) {
    (verse(first:last) |> paste(collapse = "\n"))
}

# Get a single verse of the beer song
verse <- Vectorize(function(number) {
    verse_last <- paste0(
        "No more bottles of beer on the wall, no more bottles of beer.\n",
        "Go to the store and buy some more, 99 bottles of beer on the wall.\n"
    )
    verse_template <- function(count1, count2, one = "one") {
        paste0(
            count1, " of beer on the wall, ",
            count1, " of beer.\n",
            "Take ", one, " down and pass it around, ",
            count2, " of beer on the wall.\n"
        )
    }

    if (number == 0) {
        verse_last
    } else if (number == 1) {
        verse_template("1 bottle", "no more bottles", "it")
    } else if (number == 2) {
        verse_template("2 bottles", "1 bottle")
    } else {
        verse_template(paste(number, "bottles"), paste(number - 1, "bottles"))
    }
})
