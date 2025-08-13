tournament <- function(input) {
  teams <- c(
    "Allegoric Alaskans", "Blithering Badgers",
    "Courageous Californians", "Devastating Donkeys"
  )
  matches <- rep(0, times = length(teams))
  points <- losses <- draws <- wins <- matches

  for (row in input) {
    values <- unlist(strsplit(row, ";"))
    if (length(values) != 3) {
      next
    }

    if (values[3] == "win") {
      wins[match(values[1], teams)] <- wins[match(values[1], teams)] + 1
      losses[match(values[2], teams)] <- losses[match(values[2], teams)] + 1
      points[match(values[1], teams)] <- points[match(values[1], teams)] + 3
    } else if (values[3] == "loss") {
      losses[match(values[1], teams)] <- losses[match(values[1], teams)] + 1
      wins[match(values[2], teams)] <- wins[match(values[2], teams)] + 1
      points[match(values[2], teams)] <- points[match(values[2], teams)] + 3
    } else if (values[3] == "draw") {
      draws[match(values[1], teams)] <- draws[match(values[1], teams)] + 1
      draws[match(values[2], teams)] <- draws[match(values[2], teams)] + 1
      points[match(values[1], teams)] <- points[match(values[1], teams)] + 1
      points[match(values[2], teams)] <- points[match(values[2], teams)] + 1
    } else {
      next
    }
    matches[match(values[1], teams)] <- matches[match(values[1], teams)] + 1
    matches[match(values[2], teams)] <- matches[match(values[2], teams)] + 1
  }

  tally <- data.frame(teams, matches, wins, draws, losses, points)
  tally <- tally[order(-tally$points, tally$teams), ]

  colnames(tally) <- c("Team", "MP", "W", "D", "L", "P")
  row.names(tally) <- NULL
  tally
}
