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

  def on_mercury
    return @earth_age / ORBITAL_PERIOD[:mercury]
  end

  def on_venus
    return @earth_age / ORBITAL_PERIOD[:venus]
  end

  def on_earth
    return @earth_age / ORBITAL_PERIOD[:earth]
  end

  def on_mars
    return @earth_age / ORBITAL_PERIOD[:mars]
  end

  def on_jupiter
    return @earth_age / ORBITAL_PERIOD[:jupiter]
  end

  def on_saturn
    return @earth_age / ORBITAL_PERIOD[:saturn]
  end

  def on_uranus
    return @earth_age / ORBITAL_PERIOD[:uranus]
  end

  def on_neptune
    return @earth_age / ORBITAL_PERIOD[:neptune]
  end

  private_constant :ORBITAL_PERIOD
  private_constant :EARTH_YEAR_IN_SECONDS
end
