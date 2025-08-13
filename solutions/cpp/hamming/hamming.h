#if !defined(HAMMING_H)
#define HAMMING_H

#include <string>
#include <stdexcept>

namespace hamming
{
    int compute(std::string const &a, std::string const &b);
} // namespace hamming

#endif // HAMMING_H