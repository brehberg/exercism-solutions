class Squares
  def initialize(max)
    @n = max
  end

  def square_of_sum
    ((@n + @n ** 2) / 2) ** 2
  end

  def sum_of_squares
    (@n + 3 * @n ** 2 + 2 * @n ** 3) / 6
  end

  def difference
    square_of_sum - sum_of_squares
  end
end
