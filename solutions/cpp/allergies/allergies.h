#if !defined(ALLERGIES_H)
#define ALLERGIES_H

#include <string>
#include <unordered_set>

namespace allergies
{
    using namespace std;

    class Allergy_test
    {
    private:
        unordered_set<string> allergies;

    public:
        Allergy_test(unordered_set<string>);
        bool is_allergic_to(string allergen);
        unordered_set<string> get_allergies();
    };

    Allergy_test allergy_test(int score);
} // namespace allergies

#endif // ALLERGIES_H