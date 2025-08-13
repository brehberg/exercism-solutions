class Year
  def self.leap?(year)
    divisible_by = -> (n) { year % n == 0 }

    # Occurs on every year that is evenly divisible by 4
    # except every year that is evenly divisible by 100
    # unless the year is also evenly divisible by 400.
    divisible_by.call(4) and
      not divisible_by.call(100) or
        divisible_by.call(400)
  end
end
