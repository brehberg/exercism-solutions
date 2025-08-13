#if !defined(ALLERGIES_H)
#define ALLERGIES_H

#include <string>
#include <unordered_set>

namespace allergies
{
    using namespace std;

    class allergy_test
    {
    private:
        unordered_set<string> allergies;

    public:
        allergy_test(int score);
        bool is_allergic_to(string allergen);
        const unordered_set<string> get_allergies();
    };
} // namespace allergies

#endif // ALLERGIES_H