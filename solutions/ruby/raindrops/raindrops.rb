class Raindrops
  def self.convert(number)
    divisible_by = -> (n) { number % n == 0 }

    sound = ""
    sound << "Pling" if divisible_by.call(3)
    sound << "Plang" if divisible_by.call(5)
    sound << "Plong" if divisible_by.call(7)
    sound.empty? ? number.to_s : sound
  end
end
