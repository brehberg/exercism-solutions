// Define if Pac-Man eats a ghost
pub fn eat_ghost(power_pellet_active: Bool, touching_ghost: Bool) -> Bool {
  power_pellet_active && touching_ghost
}

// Define if Pac-Man scores
pub fn score(touching_power_pellet: Bool, touching_dot: Bool) -> Bool {
  touching_power_pellet || touching_dot
}

// Define if Pac-Man loses
pub fn lose(power_pellet_active: Bool, touching_ghost: Bool) -> Bool {
  !power_pellet_active && touching_ghost
}

// Define if Pac-Man wins
pub fn win(
  has_eaten_all_dots: Bool,
  power_pellet_active: Bool,
  touching_ghost: Bool,
) -> Bool {
  has_eaten_all_dots && !lose(power_pellet_active, touching_ghost)
}
