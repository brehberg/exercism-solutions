tournament <- function(input) {
  # initialize vector work areas for tracking team results
  team_names <- c(
    "Allegoric Alaskans", "Blithering Badgers",
    "Courageous Californians", "Devastating Donkeys"
  )
  matches <- rep(0, times = length(team_names))
  points <- losses <- draws <- wins <- matches

  for (row in input) {
    # parse each line of input as "Team 1; Team 2; result"
    values <- unlist(strsplit(row, ";"))
    if (length(values) != 3) {
      next
    }
    team1 <- match(values[1], team_names)
    team2 <- match(values[2], team_names)

    if (values[3] == "win") {
      # Team 1 beat Team 2; Team 1 earns 3 points
      wins[team1] <- wins[team1] + 1
      losses[team2] <- losses[team2] + 1
      points[team1] <- points[team1] + 3
    } else if (values[3] == "loss") {
      # Team 2 beat Team 1; Team 2 earns 3 points
      wins[team2] <- wins[team2] + 1
      losses[team1] <- losses[team1] + 1
      points[team2] <- points[team2] + 3
    } else if (values[3] == "draw") {
      # Team 1 tied Team 2; both earn 1 point
      draws[team1] <- draws[team1] + 1
      draws[team2] <- draws[team2] + 1
      points[team1] <- points[team1] + 1
      points[team2] <- points[team2] + 1
    } else {
      next # listed result was not valid
    }
    matches[team1] <- matches[team1] + 1
    matches[team2] <- matches[team2] + 1
  }

  # construct data frame, sort by points descending and reset row numbers
  tally <- data.frame(
    Team = team_names, MP = matches,
    W = wins, D = draws, L = losses, P = points
  )
  tally <- tally[order(-tally$P, tally$Team), ]
  row.names(tally) <- NULL
  tally
}
