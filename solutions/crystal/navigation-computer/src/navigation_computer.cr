class Navigation
  # Navigation constants
  NEPTUNE_DISTANCE    = 4_400_000_000_i64
  MARS_DISTANCE       =   227_940_000_i32
  ATMOSPHERE_DISTANCE =        10_000_i16

  # Correct area analysis
  def correct_area_analysis(measurement)
    measurement.to_u32
  end

  # Calculate the velocity
  def calculate_velocity(distance, time)
    (distance / time).to_f32
  end
end
