class CastleDinner
  private EXPECTED_FOOD     = "Mushroom pasties"
  private EXPECTED_LETTER   = "i"
  private REPLACEMENT_DRINK = "Apple juice"

  # Check if the food is correct
  def self.check_food?(food)
    food if food.compare(EXPECTED_FOOD) == 0
  end

  # Check if the drink is poisoned
  def self.check_drink?(drink)
    drink if drink.downcase.includes?(EXPECTED_LETTER)
  end

  # Replace the drink
  def self.replace_drink(drink)
    check_drink?(drink) || REPLACEMENT_DRINK
  end
end
