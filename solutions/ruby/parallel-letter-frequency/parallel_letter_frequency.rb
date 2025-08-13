class ParallelLetterFrequency
  def self.count(texts)
    ractors = texts.map do |text|
      Ractor.new(text) do |text|
        text.gsub(/[\P{L}]/u, "").downcase.each_grapheme_cluster.tally
      end
    end

    ractors.each_with_object(Hash.new(0)) do |ractor, result|
      ractor.take.each { |letter, count| result[letter] += count }
    end
  end
end
