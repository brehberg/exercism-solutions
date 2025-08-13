class Complement
  DNA2RNA = {
    "G" => "C",
    "C" => "G",
    "T" => "A",
    "A" => "U",
  }.freeze
  private_constant :DNA2RNA

  def self.of_dna(dna)
    rna = ""
    dna.each_char do |nucleotide|
      rna << DNA2RNA[nucleotide]
    end
    return rna
  end
end
