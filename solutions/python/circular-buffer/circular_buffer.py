class BufferFullException(BufferError):
    """Exception raised when CircularBuffer is full.

    message: explanation of the error.

    """

    def __init__(self, message):
        pass


class BufferEmptyException(BufferError):
    """Exception raised when CircularBuffer is empty.

    message: explanation of the error.

    """

    def __init__(self, message):
        pass


class CircularBuffer:
    """A circular buffer, cyclic buffer or ring buffer is a data structure that
    uses a single, fixed-size buffer as if it were connected end-to-end.
    """

    def __init__(self, capacity):
        self.capacity = capacity
        self.buffer = []

    def read(self):
        """Return the first element and remove it from the buffer.
        Raise a BufferEmptyException if the buffer is empty.
        """
        if len(self.buffer) == 0:
            raise BufferEmptyException("Circular buffer is empty")
        return self.buffer.pop(0)

    def write(self, data):
        """Add the data value as the last element in the buffer.
        Raise a BufferFullException if the buffer is full.

        data: value to be added to the buffer

        """
        if len(self.buffer) == self.capacity:
            raise BufferFullException("Circular buffer is full")
        self.buffer.append(data)

    def overwrite(self, data):
        """Add the data value as the last element in the buffer.
        Remove the current first element if the buffer is full.

        data: value to be added to the buffer

        """
        if len(self.buffer) == self.capacity:
            self.read()
        self.write(data)

    def clear(self):
        """Remove all data elements from the current buffer."""
        self.buffer = []
