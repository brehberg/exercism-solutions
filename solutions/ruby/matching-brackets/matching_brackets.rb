class Brackets
  MATCHES = {
    "[" => "]",
    "{" => "}",
    "(" => ")",
  }.freeze
  private_constant :MATCHES

  # Checks that all the brackets and braces in the string are matched correctly,
  # and nested correctly
  def self.paired?(input)
    closer_needed = []

    input.each_char do |c|
      if MATCHES.key?(c)
        # opening bracket was found, add matching closing value to the stack
        closer_needed << MATCHES[c]
      elsif MATCHES.has_value?(c)
        # closing bracket was found, is it the next expected value on stack?
        return false unless closer_needed.last == c
        closer_needed.pop
      end
    end

    closer_needed.empty?
  end
end
