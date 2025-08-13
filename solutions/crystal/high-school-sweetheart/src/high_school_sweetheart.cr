class HighSchoolSweetheart
  # Clean up the name
  def self.clean_up_name(name)
    name.gsub("-", " ").strip
  end

  # Get the name's first letter
  def self.first_letter(name)
    clean_up_name(name)[0]
  end

  # Format the first letter as an initial
  def self.initial(name)
    first_letter(name).upcase + "."
  end

  def self.pair(name1, name2)
    "❤ #{initial(name1)}  +  #{initial(name2)} ❤"
  end
end
