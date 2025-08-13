class SpaceAge
  EARTH_YEAR_IN_SECONDS = 31557600.freeze
  ORBITAL_PERIOD = {
    mercury: 0.2408467,
    venus: 0.61519726,
    earth: 1.0,
    mars: 1.8808158,
    jupiter: 11.862615,
    saturn: 29.447498,
    uranus: 84.016846,
    neptune: 164.79132,
  }.freeze

  def initialize(seconds)
    @earth_age = seconds / EARTH_YEAR_IN_SECONDS.to_f
  end

  ORBITAL_PERIOD.each do |planet, planetary_period|
    define_method "on_#{planet}" do
      @earth_age / planetary_period
    end
  end

  private_constant :ORBITAL_PERIOD
  private_constant :EARTH_YEAR_IN_SECONDS
end
