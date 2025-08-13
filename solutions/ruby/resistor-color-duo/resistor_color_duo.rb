class ResistorColorDuo
  # Better Be Right Or Your Great Big Values Go Wrong
  COLORS = %w[black brown red orange yellow
              green blue violet grey white].freeze
  private_constant :COLORS

  # Calculate a resistance value from two colors
  def self.value(colors)
    COLORS.find_index(colors[0]) * 10 \
      + COLORS.find_index(colors[1])
  end
end
