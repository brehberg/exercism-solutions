class Scrabble
  POINTS = {
    1 => %w[A E I O U L N R S T],
    2 => %w[D G],
    3 => %w[B C M P],
    4 => %w[F H V W Y],
    5 => %w[K],
    8 => %w[J X],
    10 => %w[ Q Z ],
  }.freeze

  LETTER_VALUE = POINTS.each_with_object(Hash.new(0)) do |(value, letters), map|
    letters.each do |letter|
      map[letter] = value
    end
  end

  def initialize(input)
    @score = input.to_s.
      strip.upcase.chars.
      sum { |c| LETTER_VALUE[c] }
  end

  attr_reader :score
  private_constant :POINTS
end
