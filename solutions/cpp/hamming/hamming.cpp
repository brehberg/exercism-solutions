#include "hamming.h"

namespace hamming
{
    using namespace std;

    int compute(string const &a, string const &b)
    {
        if (a.size() != b.size())
        {
            throw domain_error("different lengths");
        }

        // Calculate the Hamming distance between two DNA strands.
        int difference_count{0};
        for (size_t i = 0; i < a.size(); i++)
        {
            if (a[i] != b[i])
            {
                difference_count += 1;
            }
        }
        return difference_count;
    }

} // namespace hamming
