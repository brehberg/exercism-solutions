module Hamming
  def self.compute(strand1, strand2)
    raise ArgumentError unless strand1.length == strand2.length
    strand1.chars.each_with_index.reduce(0) { |distance, (c, i)|
      distance + (c != strand2[i] ? 1 : 0)
    }
  end
end
