module Acronym
  def self.abbreviate(phrase)
    phrase.gsub(/(?<!_)\B[\w']+|[\s\W_]/, "").upcase
  end
end
