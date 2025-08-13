module Acronym
  def self.abbreviate(phrase)
    phrase.gsub(/\B[\w']+|[\s\W]/, "").upcase
  end
end
