local CircularBuffer = {}
CircularBuffer.__index = CircularBuffer

function CircularBuffer:new(size)
    local self =
        setmetatable(
        {
            capacity = size,
            values = {}
        },
        CircularBuffer
    )
    self:clear()
    return self
end

function CircularBuffer:read()
    if self.count == 0 then
        error("buffer is empty")
    end
    result = self.values[self.head]
    self.head = (self.head + 1) % self.capacity
    self.count = self.count - 1
    return result
end

function CircularBuffer:write(value)
    if value == nil then
        return
    end
    if self.count == self.capacity then
        error("buffer is full")
    end
    self:forceWrite(value)
end

function CircularBuffer:forceWrite(value)
    self.values[self.tail] = value
    self.tail = (self.tail + 1) % self.capacity
    if self.count == self.capacity then
        self.head = self.tail
    else
        self.count = self.count + 1
    end
end

function CircularBuffer:clear()
    self.count = 0
    self.head = 0
    self.tail = 0
end

return CircularBuffer
