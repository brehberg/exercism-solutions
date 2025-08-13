class Integer
  LITERALS = {
    1000 => ["M"],
    100 => ["C", "D", "M"],
    10 => ["X", "L", "C"],
    1 => ["I", "V", "X"],
  }.freeze

  def to_roman()
    num = self
    roman = ""

    LITERALS.each do |key, vals|
      unit, half, full = vals
      digit = (num / key).floor
      case digit
        when 1..3
          roman << "#{unit*digit}"
        when 4
          roman << "#{unit}#{half}"
        when 5..8
          roman << "#{half}#{unit*(digit-5)}"
        when 9
          roman << "#{unit}#{full}"
      end
      num %= key
    end

    return roman
  end
end
