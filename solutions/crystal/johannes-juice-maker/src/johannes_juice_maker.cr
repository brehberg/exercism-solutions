class JuiceMaker
  private JUICE_PER_MINUTE = 5

  # Debug light
  def self.debug_light_on?
    true
  end

  # Initialize the machine
  def initialize(amount : Int32)
    @running = false
    @fluid = amount
  end

  # Turn on the machine
  def start
    @running = true
  end

  # Status of the machine
  def running?
    @running
  end

  # Add juice
  def add_fluid(amount)
    @fluid += amount
  end

  # Turn off the machine
  def stop(minutes)
    @running = false
    @fluid -= JUICE_PER_MINUTE * minutes
  end
end
