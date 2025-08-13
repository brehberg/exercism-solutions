module Isogram
  def self.isogram?(phrase)
    !phrase.downcase.scan(/\p{Lower}/).tally.any? { |k, v| v > 1 }
  end
end
