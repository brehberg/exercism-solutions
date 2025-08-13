module Chess
  # Define rank & file range
  RANKS = 1..8.freeze
  FILES = "A".."H".freeze

  # Check if square is valid
  def self.valid_square?(rank, file)
    RANKS.include?(rank) && FILES.to_a.include?(file)
  end

  # Get player's nickname
  def self.nick_name(first_name, last_name)
    "#{first_name[...2]}#{last_name[-2..]}".upcase
  end

  # Create move message
  def self.move_message(first_name, last_name, square)
    name = nick_name(first_name, last_name)

    valid_square?(square[1].to_i, square[0]) ?
      "#{name} moved to #{square}" :
      "#{name} attempted to move to #{square}, but that is not a valid square"
  end
end
