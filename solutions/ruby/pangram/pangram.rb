class Pangram
  LETTERS_IN_ALPHABET = 26.freeze

  def self.pangram?(sentence)
    sentence.downcase.scan(/[a-z]/).uniq.count == LETTERS_IN_ALPHABET
  end
end
