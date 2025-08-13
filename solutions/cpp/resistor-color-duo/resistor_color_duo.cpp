#include "resistor_color_duo.h"

#include <algorithm>

namespace resistor_color_duo
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

    int value(vector<string> colors)
    {
        return color_code(colors[0]) * 10 + color_code(colors[1]);
    }
} // namespace resistor_color_duo
