<#
.SYNOPSIS
Implement the circular buffer data structure.

.DESCRIPTION
A circular buffer, cyclic buffer or ring buffer is a data structure that uses a single, fixed-size buffer as if it were connected end-to-end.
Please implement the circular buffer class with these methods:
- Write     : write new value into the buffer, raise error if the buffer is full.
- Overwrite : overwrite the oldest element in the buffer if the buffer is full, otherwise behave like write.
- Clear     : clear all elements in the buffer, it is now empty.
- Read      : read the oldest element in the buffer, and return its value.

.EXAMPLE
$buffer = [CircularBuffer]::new(2)

$buffer.Write(1)
$buffer.Read()
Return: 1

$buffer.Write(2)
$buffer.Write(3)
$buffer.Overwrite(5)
$buffer.Read()
Return: 5

$buffer.Clear()
$buffer.Read()
Throw "BufferError: Circular buffer is empty"
#>

Class CircularBuffer {
    [int]hidden $head = -1
    [int]hidden $tail = -1
    [int]hidden $size
    [object[]]hidden $values

    CircularBuffer([int]$Size) {
        $this.size = $Size
        $this.values = [object[]]::new($Size)
    }

    [void] Write([object]$Value) {
        if ($this.IsFull()) {
            Throw "BufferError: Circular buffer is full"
        }
        if ($this.IsEmpty()) {
            $this.tail = $this.head = 0
        }
        else {
            $this.tail = ($this.tail + 1) % $this.size
        }
        $this.values[$this.tail] = $Value
    }

    [void] Overwrite([object]$Value) {
        if ($this.IsFull()) {
            $this.tail = $this.head
            $this.head = ($this.tail + 1) % $this.size
            $this.values[$this.tail] = $Value
        }
        else {
            $this.Write($Value)
        }
    }

    [object] Read() {        
        if ($this.IsEmpty()) {
            Throw "BufferError: Circular buffer is empty"
        }
        $result = $this.values[$this.head]
        $this.values[$this.head] = $null
        if ($this.head -eq $this.tail) {            
            $this.Clear()  # final element was removed from buffer
        }
        else {
            $this.head = ($this.head + 1) % $this.size
        }
        return $result
    }

    [void] Clear() {
        $this.head = $this.tail = -1
        $this.values = @($null) * $this.size
    }

    [boolean] IsEmpty() {
        return ($this.head -lt 0)
    }    
    [boolean] IsFull() {
        return ($this.head -eq ($this.tail + 1) % $this.size)           
    }    
}

