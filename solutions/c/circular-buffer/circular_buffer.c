#include "circular_buffer.h"

#include <stdlib.h>
#include <errno.h>

struct circular_buffer_s
{
    buffer_value_t *_values;
    size_t _capacity;
    size_t _count;
    size_t _head;
    size_t _tail;
};

circular_buffer_t *new_circular_buffer(size_t capacity)
{
    circular_buffer_t *buffer = malloc(sizeof(circular_buffer_t));
    buffer->_capacity = capacity;
    buffer->_values = malloc(sizeof(buffer_value_t) * capacity);
    clear_buffer(buffer);
    return buffer;
}

void delete_buffer(circular_buffer_t *buffer)
{
    free(buffer->_values);
    free(buffer);
}

int16_t read(circular_buffer_t *buffer, buffer_value_t *value)
{
    if (buffer->_count == 0)
    {
        errno = ENODATA;
        return EXIT_FAILURE;
    }
    *value = buffer->_values[buffer->_head];
    buffer->_head = (buffer->_head + 1) % buffer->_capacity;
    buffer->_count -= 1;
    return EXIT_SUCCESS;
}

int16_t write(circular_buffer_t *buffer, buffer_value_t value)
{
    if (buffer->_count == buffer->_capacity)
    {
        errno = ENOBUFS;
        return EXIT_FAILURE;
    }
    return overwrite(buffer, value);
}

int16_t overwrite(circular_buffer_t *buffer, buffer_value_t value)
{
    buffer->_values[buffer->_tail] = value;
    buffer->_tail = (buffer->_tail + 1) % buffer->_capacity;
    buffer->_count == buffer->_capacity ? buffer->_head = buffer->_tail : buffer->_count++;
    return EXIT_SUCCESS;
}

void clear_buffer(circular_buffer_t *buffer)
{
    buffer->_count = 0;
    buffer->_head = 0;
    buffer->_tail = 0;
}