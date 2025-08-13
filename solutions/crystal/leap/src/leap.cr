module Year
  def self.leap?(year : Number) : Bool
    is_divisible_by = ->(n : Int32) : Bool { year % n == 0 }
    is_divisible_by.call(4) &&
      !is_divisible_by.call(100) ||
      is_divisible_by.call(400)
  end
end
