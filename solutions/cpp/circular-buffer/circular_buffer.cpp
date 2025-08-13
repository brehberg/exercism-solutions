#include "circular_buffer.h"
#include <stdexcept>
#include <string>

using namespace std;

namespace circular_buffer
{
    template <typename T>
    T circular_buffer<T>::read()
    {
        if (m_count == 0)
        {
            throw(domain_error(""));
        }
        T result = m_buffer[m_head];
        m_head = (m_head + 1) % m_capacity;
        m_count -= 1;
        return result;
    }

    template <typename T>
    void circular_buffer<T>::write(T value)
    {
        if (m_count == m_capacity)
        {
            throw(domain_error(""));
        }
        overwrite(value);
    }

    template <typename T>
    void circular_buffer<T>::overwrite(T value)
    {
        m_buffer[m_tail] = value;
        m_tail = (m_tail + 1) % m_capacity;
        (m_count == m_capacity) ? m_head = m_tail : m_count += 1;
    }

    template <typename T>
    void circular_buffer<T>::clear()
    {
        m_count = m_head = m_tail = 0;
    }

} // namespace circular_buffer

template struct circular_buffer::circular_buffer<int>;
template struct circular_buffer::circular_buffer<string>;