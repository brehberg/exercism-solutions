module ProteinTranslation
  private STOP_CODON = "STOP"

  def self.proteins(strand : String) : Array(String)
    strand.chars.each_slice(3).with_object([] of String) do |chunk, result|
      amino_acid = from_codon(chunk.join)
      return result if amino_acid == STOP_CODON
      result << amino_acid
    end
  end

  private def self.from_codon(codon : String) : String
    case codon
    when "AUG"                      then "Methionine"
    when "UUU", "UUC"               then "Phenylalanine"
    when "UUA", "UUG"               then "Leucine"
    when "UCU", "UCC", "UCA", "UCG" then "Serine"
    when "UAU", "UAC"               then "Tyrosine"
    when "UGU", "UGC"               then "Cysteine"
    when "UGG"                      then "Tryptophan"
    when "UAA", "UAG", "UGA"        then STOP_CODON
    else                                 raise ArgumentError.new
    end
  end
end
