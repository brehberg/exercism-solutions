module Chess
  # Define rank & file range
  RANKS = 1..8
  FILES = 'A'..'H'

  # Check if square is valid
  def self.valid_square?(rank, file)
    RANKS.includes?(rank) && FILES.includes?(file)
  end

  # Get player's nickname
  def self.nickname(first_name, last_name)
    (first_name[0, 2] + last_name[-2, 2]).upcase
  end

  # Create move message
  def self.move_message(first_name, last_name, square)
    name = nickname(first_name, last_name)
    return "#{name} moved to #{square}" if valid_square?(square[1].to_i, square[0]) # message for valid move
    "#{name} attempted to move to #{square}, but that is not a valid square"        # message for invalid move
  end
end
