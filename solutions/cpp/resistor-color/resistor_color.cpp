#include "resistor_color.h"

#include <algorithm>

namespace resistor_color
{
    using namespace std;

    // Better Be Right Or Your Great Big Values Go Wrong
    const vector<string> color_codes{
        "black", "brown", "red", "orange", "yellow",
        "green", "blue", "violet", "grey", "white"};

    int color_code(string color)
    {
        auto it = find(color_codes.begin(), color_codes.end(), color);
        return distance(color_codes.begin(), it);
    }

    vector<string> colors()
    {
        return color_codes;
    }

} // namespace resistor_color
