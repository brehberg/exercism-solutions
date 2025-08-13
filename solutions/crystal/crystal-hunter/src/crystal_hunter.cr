class Rules
  # Define if character gets bonus points
  def bonus_points?(power_up_active, touching_bandit)
    power_up_active && touching_bandit
  end

  # Define if character scores
  def score?(touching_power_up, touching_crystal)
    touching_power_up || touching_crystal
  end

  # Define if character loses
  def lose?(power_up_active, touching_bandit)
    !power_up_active && touching_bandit
  end

  # Define if character wins
  def win?(has_picked_up_all_crystals, power_up_active, touching_bandit)
    has_picked_up_all_crystals && !lose?(power_up_active, touching_bandit)
  end
end
