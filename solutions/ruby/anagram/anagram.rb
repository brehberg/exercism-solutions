class Anagram
  def initialize(target)
    @base = target.downcase
    @base_sorted = @base.chars.sort
  end

  def is_anagram?(candidate)
    return false unless candidate.length == @base.length
    word = candidate.downcase
    return false if word == @base
    return word.chars.sort == @base_sorted
  end

  def match(candidates)
    candidates.select { |word| is_anagram?(word) }
  end
end
