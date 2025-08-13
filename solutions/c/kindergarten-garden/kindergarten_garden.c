#include "kindergarten_garden.h"
#include <string.h>

static plant_t decode_plant(char code)
{
    switch (code)
    {
    case 'G':
        return GRASS;
    case 'C':
        return CLOVER;
    case 'R':
        return RADISHES;
    case 'V':
        return VIOLETS;
    default:
        return UNKNOWN;
    }
}

static const char *students[] = {
    "Alice",
    "Bob",
    "Charlie",
    "David",
    "Eve",
    "Fred",
    "Ginny",
    "Harriet",
    "Ileana",
    "Joseph",
    "Kincaid",
    "Larry",
};

plants_t plants(const char *diagram, const char *student)
{
    int offset;
    size_t length = sizeof(students) / sizeof(*students);
    for (size_t i = 0; i < length; ++i)
    {
        if (strcmp(students[i], student) == 0)
        {
            offset = 2 * i;
            break;
        }
    }

    const char *row1 = diagram;
    const char *row2 = strchr(diagram, '\n') + 1;

    return (plants_t){
        .plants = {
            decode_plant(row1[offset]),
            decode_plant(row1[offset + 1]),
            decode_plant(row2[offset]),
            decode_plant(row2[offset + 1]),
        }};
}