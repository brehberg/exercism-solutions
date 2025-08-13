class ListOps
  def self.arrays(list)
    count = 0
    list.each do
      count += 1
    end
    count
  end

  def self.reverser(list)
    result = []
    list.each do |item|
      result.unshift(item)
    end
    result
  end

  def self.concatter(list1, list2)
    result = []
    list1.each do |item|
      result << item
    end
    list2.each do |item|
      result << item
    end
    result
  end

  def self.mapper(list)
    result = []
    list.each do |item|
      result << yield(item)
    end
    result
  end

  def self.filterer(list)
    result = []
    list.each do |item|
      result << item if yield(item)
    end
    result
  end

  def self.sum_reducer(list)
    result = 0
    list.each do |item|
      result += item
    end
    result
  end

  def self.factorial_reducer(list)
    result = 1
    list.each do |item|
      result *= item
    end
    result
  end
end
