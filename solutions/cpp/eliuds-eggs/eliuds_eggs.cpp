#include "eliuds_eggs.h"

namespace chicken_coop
{
    unsigned int positions_to_quantity(unsigned int encoded)
    {
        unsigned int count{0};
        while (encoded)
        {
            count += encoded & 1;
            encoded >>= 1;
        }
        return count;
    }

} // namespace chicken_coop
