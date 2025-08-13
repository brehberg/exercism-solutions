class Integer
  LITERALS = [["M", "", ""],
              ["C", "D", "M"],
              ["X", "L", "C"],
              ["I", "V", "X"]].freeze

  def to_roman()
    num = self
    value = 1000
    roman = ""

    LITERALS.each do |unit, half, full|
      digit = (num / value).floor
      case digit
      when 0..3
        roman << "#{unit * digit}"
      when 4
        roman << "#{unit}#{half}"
      when 5..8
        roman << "#{half}#{unit * (digit - 5)}"
      when 9
        roman << "#{unit}#{full}"
      end
      num %= value
      value /= 10
    end

    return roman
  end
end
