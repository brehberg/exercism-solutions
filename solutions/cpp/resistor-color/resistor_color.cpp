#include "resistor_color.h"

#include <algorithm>

namespace resistor_color
{
    using namespace std;

    vector<string> colors()
    {
        // Better Be Right Or Your Great Big Values Go Wrong
        return {"black", "brown", "red", "orange", "yellow",
                "green", "blue", "violet", "grey", "white"};
    }

    int color_code(string color)
    {
        vector<string> codes = colors();
        auto it = find(codes.begin(), codes.end(), color);
        return it - codes.begin();
    }

} // namespace resistor_color
