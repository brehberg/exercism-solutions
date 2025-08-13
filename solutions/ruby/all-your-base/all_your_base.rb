class BaseConverter
  def self.convert(from_base, from_digits, to_base)
    raise ArgumentError.new "invalid input base" unless from_base >= 2
    raise ArgumentError.new "invalid output base" unless to_base >= 2

    # convert sequence of digits in input base to whole integer value
    value = from_digits.each.with_index(1).reduce(0) { |sum, (d, i)|
      raise ArgumentError.new "negative digits not allowed" unless d >= 0
      raise ArgumentError.new "digit out of range" unless d < from_base
      sum + d * from_base ** (from_digits.length - i)
    }

    # convert whole integer value to sequence of digits in output base
    to_digits = []
    until value < to_base
      to_digits.prepend value % to_base
      value = value / to_base
    end
    to_digits.prepend value
  end
end
