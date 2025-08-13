class Isogram
  def self.isogram?(input)
    !input.downcase.scan(/[a-z]/).tally.any? { |k, v| v > 1 }
  end
end
