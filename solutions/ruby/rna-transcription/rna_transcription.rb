class Complement
  DNA2RNA = {
    "G" => "C",
    "C" => "G",
    "T" => "A",
    "A" => "U",
  }.freeze
  private_constant :DNA2RNA

  def self.of_dna(dna)
    dna.chars.map { |nucleotide| DNA2RNA[nucleotide] }.join
  end
end
