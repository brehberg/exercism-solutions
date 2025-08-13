class Translation
  STOP_CODON = "STOP".freeze
  private_constant :STOP_CODON

  def self.of_rna(strand)
    strand.chars.each_slice(3).with_object([]) do |chunk, result|
      protein = of_codon(chunk.join)
      return result if protein == STOP_CODON
      result << protein
    end
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
end

class InvalidCodonError < StandardError; end
