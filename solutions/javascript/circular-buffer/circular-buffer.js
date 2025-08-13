class CircularBuffer {
  #head = -1;
  #tail = -1;
  #size = 0;
  #values;

  constructor(size) {
    this.#size = size;
    this.#values = new Array(size);
  }

  write(value) {
    if (this.#isFull()) {
      throw new BufferFullError();
    }
    if (this.#isEmpty()) {
      this.#tail = this.#head = 0;
    } else {
      this.#tail = (this.#tail + 1) % this.#size;
    }
    this.#values[this.#tail] = value;
  }

  read() {
    if (this.#isEmpty()) {
      throw new BufferEmptyError();
    }
    let result = this.#values[this.#head];
    this.#values[this.#head] = undefined;
    if (this.#head == this.#tail) {
      this.clear(); // final element was removed from buffer
    } else {
      this.#head = (this.#head + 1) % this.#size;
    }
    return result;
  }

  forceWrite(value) {
    if (this.#isFull()) {
      this.#tail = this.#head;
      this.#head = (this.#tail + 1) % this.#size;
      this.#values[this.#tail] = value;
    } else {
      this.write(value);
    }
  }

  clear() {
    this.#head = this.#tail = -1;
    this.#values.fill(undefined);
  }

  #isEmpty() {
    return this.#head < 0;
  }

  #isFull() {
    return this.#head === (this.#tail + 1) % this.#size;
  }
}

export default CircularBuffer;

export class BufferFullError extends Error {
  constructor() {
    super("BufferError: Circular buffer is full");
  }
}

export class BufferEmptyError extends Error {
  constructor() {
    super("BufferError: Circular buffer is empty");
  }
}
