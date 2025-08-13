#if !defined(PARALLEL_LETTER_FREQUENCY_H)
#define PARALLEL_LETTER_FREQUENCY_H

#include <vector>
#include <string_view>
#include <unordered_map>

namespace parallel_letter_frequency
{
    using texts_t = std::vector<std::string_view>;
    using map_t = std::unordered_map<char, int>;
    map_t frequency(const texts_t &texts);
}

#endif
