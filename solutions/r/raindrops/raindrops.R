# raindrops returns a string that contains sounds corresponding to factors.
raindrops <- function(number) {
  # is_divisible returns if a given number is evenly divisible by d.
  is_divisible <- function(d) {
    number %% d == 0
  }

  # The rules of raindrops are that if a given number:
  # 	has 3 as a factor, add 'Pling' to the result.
  # 	has 5 as a factor, add 'Plang' to the result.
  # 	has 7 as a factor, add 'Plong' to the result.
  # 	does not have any of 3, 5, or 7 as a factor, result should be the number.
  drops <- c(Pling = 3, Plang = 5, Plong = 7)

  sound <- paste0(names(drops[is_divisible(drops)]), collapse = "")
  ifelse((nchar(sound)), sound, as.character(number))
}
