class Nucleotide
  def self.from_dna(dna)
    @tally = dna.chars.tally
  end
  def self.count(nucleotide)
    tally[nucleotide]
  end
  def self.histogram
    tally
  end
end
