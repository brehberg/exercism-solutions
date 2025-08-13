struct Int
  ROMAN_LITERALS = {
    1_000 => "M",
      900 => "CM",
      500 => "D",
      400 => "CD",
      100 => "C",
       90 => "XC",
       50 => "L",
       40 => "XL",
       10 => "X",
        9 => "IX",
        5 => "V",
        4 => "IV",
        1 => "I",
  }

  def to_roman
    current = self

    String.build do |roman|
      ROMAN_LITERALS.each do |value, literal|
        while current >= value
          roman << literal
          current -= value
        end
      end
    end
  end
end
