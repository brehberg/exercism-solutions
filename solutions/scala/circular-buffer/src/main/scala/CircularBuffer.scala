class EmptyBufferException() extends Exception {}

class FullBufferException() extends Exception {}

class CircularBuffer(var capacity: Int) {
  private var count: Int = 0
  private var head: Int = 0
  private var tail: Int = 0
  private var buffer = new Array[Int](capacity)

  def write(value: Int) = {
    if(count == capacity) throw new FullBufferException
    overwrite(value)
  }

  def read(): Int = {
    if(count == 0) throw new EmptyBufferException
    val value: Int = buffer(tail)
    tail = (tail + 1) % capacity
    count -= 1
    value
  }

  def overwrite(value: Int) = {
    buffer(head) = value
    head = (head + 1) % capacity
    if(count == capacity)
      tail = head
    else
      count += 1
  }

  def clear() = {
    count = 0
    head = 0
    tail = 0
  }
}