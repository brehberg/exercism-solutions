#include "eliuds_eggs.h"

namespace chicken_coop
{
    unsigned int positions_to_quantity(unsigned int encoded)
    {
        unsigned int count{0};
        while (encoded)
        {
            encoded &= (encoded - 1);
            count++;
        }
        return count;
    }

} // namespace chicken_coop
