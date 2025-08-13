class EmptyBufferException extends Exception {}
class FullBufferException extends Exception {}

class CircularBuffer {
    def values
    def capacity
    def count
    def head
    def tail

    CircularBuffer(int capacity) {
        this.capacity = capacity
        this.values = new ArrayList(capacity)        
        this.clear()
    }

    def clear() {
        this.count = 0
        this.head = 0
        this.tail = 0
    }

    def read() {
        if (this.count == 0) {
            throw new EmptyBufferException()
        }
        def value = this.values[this.head]
        this.head = (this.head + 1) % this.capacity
        this.count -= 1
        return value
    }

    def write(int item) {
        if (this.count == this.capacity) {
            throw new FullBufferException()
        }
        overwrite(item)
    }

    def overwrite(int item) {
        this.values[this.tail] = item        
        this.tail = (this.tail + 1) % this.capacity
        this.count == this.capacity ? this.head = this.tail : this.count++
    }
}
