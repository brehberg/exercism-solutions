import strUtils
import seqUtils
import sets

proc isPangram*(sentence: string): bool =
  LowercaseLetters.toSeq.toHashSet <= sentence.toLowerAscii.toHashSet
