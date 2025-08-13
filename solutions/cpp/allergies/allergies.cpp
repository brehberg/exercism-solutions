#include "allergies.h"

namespace allergies
{
    using namespace std;

    static const string allergens[8] = {
        "eggs",
        "peanuts",
        "shellfish",
        "strawberries",
        "tomatoes",
        "chocolate",
        "pollen",
        "cats",
    };

    allergy_test::allergy_test(int score)
    {
        int value = 1;
        for (const string &allergen : allergens)
        {
            if (score & value)
            {
                allergies.insert(allergen);
            }
            value = value << 1;
        }
    }

    bool allergy_test::is_allergic_to(string allergen)
    {
        return allergies.find(allergen) != allergies.end();
    }

    const unordered_set<string> allergy_test::get_allergies()
    {
        return allergies;
    }
} // namespace allergies
