#if !defined(CIRCULAR_BUFFER_H)
#define CIRCULAR_BUFFER_H

namespace circular_buffer
{
    template <typename T>
    struct circular_buffer
    {
    public:
        circular_buffer(int capacity) : m_capacity(capacity)
        {
            m_buffer = new T[capacity];
        };
        ~circular_buffer()
        {
            delete[] m_buffer;
        }
        T read();
        void write(T entry);
        void overwrite(T entry);
        void clear();

    private:
        T *m_buffer;
        int m_capacity;
        int m_count{0};
        int m_head{0};
        int m_tail{0};
    };
} // namespace circular_buffer

#endif // CIRCULAR_BUFFER_H