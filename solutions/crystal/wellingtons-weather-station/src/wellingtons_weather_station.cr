class Temperature
  private SENSORS_PER_UNIT = 4

  # Convert the temperature from Celsius to Kelvin
  def to_kelvin(celsius)
    celsius + 273.15
  end

  # Round the temperature
  def round(celsius)
    celsius.round(1)
  end

  # Convert the temperature from Celsius to Fahrenheit
  def to_fahrenheit(celsius)
    (celsius * 1.8 + 32).to_i
  end

  # Get the number of missing sensors
  def number_missing_sensors(number_of_sensors)
    (SENSORS_PER_UNIT - number_of_sensors) % SENSORS_PER_UNIT
  end
end
