class ResistorColorDuo
  # Calculate a resistance value from two colors
  def self.value(colors)
    color_code(colors[0]) * 10 + color_code(colors[1])
  end

  # Return the value of a color band
  def self.color_code(color)
    COLORS.find_index(color)
  end

  # Better Be Right Or Your Great Big Values Go Wrong
  COLORS = %w[black brown red orange yellow
              green blue violet grey white].freeze
  private_constant :COLORS
end
