pub fn reply(guess: Int) -> String {
  case guess {
    42 -> "Correct"           // Reply to a correct guess
    i if i < 41 -> "Too low"  // Reply to too low guesses
    i if i > 43 -> "Too high" // Reply to too high guesses
    _ ->  "So close"          // Reply to a close guess
  }
}
