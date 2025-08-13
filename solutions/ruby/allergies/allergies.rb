class Allergies
  ALLERGENS = {
    "eggs" => 1,
    "peanuts" => 2,
    "shellfish" => 4,
    "strawberries" => 8,
    "tomatoes" => 16,
    "chocolate" => 32,
    "pollen" => 64,
    "cats" => 128,
  }.freeze

  def initialize(score)
    @score = score
  end

  def allergic_to?(allergen)
    @score & ALLERGENS[allergen] > 0
  end

  def list
    ALLERGENS.keys.filter { |allergen| allergic_to?(allergen) }
  end
end
