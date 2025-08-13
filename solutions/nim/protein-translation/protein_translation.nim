proc translate*(s: string): seq[string] =
  for i in countup(0, s.high, 3):
    case s[i..<min(i+3, s.len)]:
      of "AUG": result.add("Methionine")
      of "UUU", "UUC": result.add("Phenylalanine")
      of "UUA", "UUG": result.add("Leucine")
      of "UCU", "UCC", "UCA", "UCG": result.add("Serine")
      of "UAU", "UAC": result.add("Tyrosine")
      of "UGU", "UGC": result.add("Cysteine")
      of "UGG": result.add("Tryptophan")
      of "UAA", "UAG", "UGA": break
      else: raise newException(ValueError, "invalid codon")
