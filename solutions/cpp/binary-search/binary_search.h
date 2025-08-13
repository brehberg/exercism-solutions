#if !defined(BINARY_SEARCH_H)
#define BINARY_SEARCH_H
#include <vector>
#include <stdexcept>

namespace binary_search
{
    std::size_t find(const std::vector<int>, const int);
} // namespace binary_search

#endif // BINARY_SEARCH_H