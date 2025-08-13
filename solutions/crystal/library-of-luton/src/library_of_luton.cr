class Library
  # Get first letter of title
  def self.first_letter(title : String) : Char
    title[0]
  end

  # Get authors initials
  def self.initials(first_name : String, last_name : String) : String
    "#{first_letter(first_name)}.#{first_letter(last_name)}"
  end

  # Decrypt letter
  def self.decrypt_character(character : Char) : (Char | Nil)
    character.upcase == 'A' ? character + 25 : character.pred if character.letter?
  end

  # Decrypt text
  def self.decrypt_text(text : String) : String
    text.chars.each.map { |c| decrypt_character(c) || c }.join
  end
end
