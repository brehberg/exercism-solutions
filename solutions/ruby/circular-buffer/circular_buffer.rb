class CircularBuffer
  def initialize(capacity)
    @count = @head = @tail = 0
    @capacity = capacity
    @buffer = Array.new(capacity)
  end

  def read
    raise BufferEmptyException if @count == 0
    value = @buffer[@tail]
    @tail = (@tail + 1) % @capacity
    @count -= 1
    value
  end

  def write(value)
    raise BufferFullException if @count == @capacity
    write!(value)
  end

  def write!(value)
    @buffer[@head] = value
    @head = (@head + 1) % @capacity
    @count == @capacity ? @tail = @head : @count += 1
  end

  def clear
    @count = @head = @tail = 0
  end

  class BufferEmptyException < StandardError; end
  class BufferFullException < StandardError; end
end
