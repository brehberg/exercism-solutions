#include "binary_search.h"

namespace binary_search
{
    using namespace std;

    size_t find(const vector<int> &list, const int key)
    {
        int low = 0, high = list.size() - 1;

        while (low <= high)
        {
            size_t mid = low + (high - low) / 2;
            int value = list[mid];

            if (value > key)
                high = mid - 1;
            else if (value < key)
                low = mid + 1;
            else
                return mid;
        }
        throw domain_error("not found");
    }
} // namespace binary_search
