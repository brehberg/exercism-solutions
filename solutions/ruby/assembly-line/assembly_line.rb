class AssemblyLine
  def initialize(speed)
    @speed = speed
  end

  # Calculate the production rate per hour
  def production_rate_per_hour    
    success_rate / 100.0 * CARS_PER_HOUR * @speed
  end

  def working_items_per_minute
    (production_rate_per_hour / 60).floor
  end

  CARS_PER_HOUR = 221.freeze
  private_constant :CARS_PER_HOUR
  
  private
  def success_rate    
    return 100 if @speed >= 1 and @speed <= 4
    return 90 if @speed >= 5 and @speed <= 8
    return 80 if @speed == 9
    return 77 if @speed == 10
    return 0 # default (off)
  end  
end
