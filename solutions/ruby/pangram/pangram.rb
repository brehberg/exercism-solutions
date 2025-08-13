module Pangram
  LETTERS_IN_ALPHABET = 26.freeze

  def self.pangram?(sentence)
    sentence.downcase.scan(/\p{Lower}/).uniq.count == LETTERS_IN_ALPHABET
  end
end
