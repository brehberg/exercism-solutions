#include "binary_search.h"

namespace binary_search
{
    using namespace std;

    size_t find(const vector<int> list, const int key)
    {
        int low = 0;
        int high = list.size() - 1;

        while (low <= high)
        {
            int mid = (low + high) / 2;
            if (list[mid] > key)
            {
                high = mid - 1;
            }
            else if (list[mid] < key)
            {
                low = mid + 1;
            }
            else
            {
                return mid;
            }
        }
        throw domain_error("not found");
    }
} // namespace binary_search
