class BirdCount
  BUSY_DAY = 5
  private_constant :BUSY_DAY

  # Check what the counts were last week
  def self.last_week
    [0, 2, 5, 3, 7, 8, 4]
  end

  def initialize(birds_per_day)
    @birds_per_day = birds_per_day
  end

  # Check how many birds visited yesterday
  def yesterday
    @birds_per_day[-2]
  end

  # Calculate the total number of visiting birds
  def total
    @birds_per_day.sum
  end

  # Calculate the number of busy days
  def busy_days
    @birds_per_day.count { |day| day >= BUSY_DAY }
  end

  # Check if there was a day with no visiting birds
  def day_without_birds?
    @birds_per_day.any? { |day| day == 0 }
  end
end
