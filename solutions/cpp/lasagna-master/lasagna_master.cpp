#include "lasagna_master.h"

namespace lasagna_master
{
    using namespace std;

    const int gramsOfNoodlesPerLayer{50};
    const double litersOfSaucePerLayer{0.2};

    int preparationTime(const vector<string> &layers, int timePerLayer)
    {
        return layers.size() * timePerLayer;
    }

    amount quantities(const std::vector<std::string> &layers)
    {
        amount needed{0, 0.0};
        for (size_t i{0}; i < layers.size(); ++i)
        {
            if (layers[i] == "noodles")
            {
                needed.noodles += gramsOfNoodlesPerLayer;
            }
            else if (layers[i] == "sauce")
            {
                needed.sauce += litersOfSaucePerLayer;
            }
        }
        return needed;
    };

    void addSecretIngredient(vector<string> &myList, const vector<string> &friendsList)
    {
        myList.pop_back();
        myList.emplace_back(friendsList.back());
    };

    vector<double> scaleRecipe(const vector<double> &quantities, int portions)
    {
        vector<double> scaledQuantities(quantities.size());

        for (size_t i{0}; i < quantities.size(); ++i)
        {
            scaledQuantities[i] = quantities[i] * portions / 2;
        }
        return scaledQuantities;
    }

    void addSecretIngredient(vector<string> &myList, const string secretIngredient)
    {
        addSecretIngredient(myList, vector<string>{secretIngredient});
    };

} // namespace lasagna_master
