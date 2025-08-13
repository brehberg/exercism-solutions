package circular

// Implement a circular buffer of bytes supporting both overflow-checked writes
// and unconditional, possibly overwriting, writes.
//
// We chose the provided API so that Buffer implements io.ByteReader
// and io.ByteWriter and can be used (size permitting) as a drop in
// replacement for anything using that interface.

import "errors"

type Buffer struct {
	values []byte
	count  int
	head   int
	tail   int
}

var (
	errEmptyBuffer = errors.New("circular buffer is empty")
	errFullBuffer  = errors.New("circular buffer is full")
)

func NewBuffer(size int) *Buffer {
	b := &Buffer{values: make([]byte, size)}
	b.Reset()
	return b
}

func (b *Buffer) ReadByte() (byte, error) {
	if b.count == 0 {
		return 0, errEmptyBuffer
	}
	result := b.values[b.head]
	b.head = (b.head + 1) % cap(b.values)
	b.count -= 1
	return result, nil
}

func (b *Buffer) WriteByte(c byte) error {
	if b.count == cap(b.values) {
		return errFullBuffer
	}
	b.Overwrite(c)
	return nil
}

func (b *Buffer) Overwrite(c byte) {
	b.values[b.tail] = c
	b.tail = (b.tail + 1) % cap(b.values)
	if b.count == cap(b.values) {
		b.head = b.tail
	} else {
		b.count += 1
	}
}

func (b *Buffer) Reset() {
	b.count = 0
	b.head = 0
	b.tail = 0
}
