class Nucleotide
  def self.from_dna(input)
    raise ArgumentError if input.match(/[^GCTA]/)
    Nucleotide.new(input)
  end

  def initialize(strand)
    @tally = strand.chars.tally(
      { "G" => 0, "C" => 0, "T" => 0, "A" => 0 }
    )
  end

  def count(nucleotide)
    @tally[nucleotide]
  end

  def histogram
    @tally
  end
end
