class Translation
  STOP_CODON = "STOP".freeze
  private_constant :STOP_CODON

  def self.of_rna(strand)
    proteins = []
    chunk_every(strand, 3).each do |chunk|
      protein = of_codon(chunk)
      break if protein == STOP_CODON
      proteins.append(protein)
    end
    return proteins
  end

  private

  def self.of_codon(codon)
    case codon
    when "AUG"
      return "Methionine"
    when "UUU", "UUC"
      return "Phenylalanine"
    when "UUA", "UUG"
      return "Leucine"
    when "UCU", "UCC", "UCA", "UCG"
      return "Serine"
    when "UAU", "UAC"
      return "Tyrosine"
    when "UGU", "UGC"
      return "Cysteine"
    when "UGG"
      return "Tryptophan"
    when "UAA", "UAG", "UGA"
      return STOP_CODON
    else
      raise InvalidCodonError
    end
  end

  def self.chunk_every(string, size)
    (0..(string.length - 1) / size).map { |i| string[i * size, size] }
  end
end

class InvalidCodonError < StandardError
end
