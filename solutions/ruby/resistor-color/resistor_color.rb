class ResistorColor
  # Better Be Right Or Your Great Big Values Go Wrong
  COLORS = %w[black brown red orange yellow green blue violet grey white].freeze

  # Return the value of a color band
  def self.color_code(color)
    COLORS.find_index(color)
  end
end
