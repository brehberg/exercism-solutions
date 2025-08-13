#include "allergies.h"

namespace allergies
{
    using namespace std;

    string allergens[] = {
        "eggs",
        "peanuts",
        "shellfish",
        "strawberries",
        "tomatoes",
        "chocolate",
        "pollen",
        "cats",
    };

    Allergy_test allergy_test(int score)
    {
        unordered_set<string> allergies;
        int value = 1;
        for (const string &allergen : allergens)
        {
            if ((score & value) > 0)
            {
                allergies.insert(allergen);
            }
            value = value << 1;
        }
        return {allergies};
    }

    Allergy_test::Allergy_test(unordered_set<string> allergies)
        : allergies{allergies} {}

    bool Allergy_test::is_allergic_to(string allergen)
    {
        return allergies.find(allergen) != allergies.end();
    }

    unordered_set<string> Allergy_test::get_allergies()
    {
        return allergies;
    }
} // namespace allergies
