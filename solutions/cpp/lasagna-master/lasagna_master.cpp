#include "lasagna_master.h"

#include <algorithm>

namespace lasagna_master
{
    using namespace std;

    int preparationTime(const vector<string> &layers, int timePerLayer)
    {
        return layers.size() * timePerLayer;
    }

    amount quantities(const std::vector<std::string> &layers)
    {
        amount needed{0, 0.0};
        for (const string &layer : layers)
        {
            if (layer == "noodles")
            {
                needed.noodles += gramsOfNoodlesPerLayer;
            }
            else if (layer == "sauce")
            {
                needed.sauce += litersOfSaucePerLayer;
            }
        }
        return needed;
    };

    vector<double> scaleRecipe(const vector<double> &quantities, int portions)
    {
        double scaledPortions{portions / (double)standardRecipePortions};
        vector<double> scaledQuantities(quantities.size());

        transform(quantities.begin(), quantities.end(), scaledQuantities.begin(),
                  [scaledPortions](double quantity)
                  { return quantity * scaledPortions; });

        return scaledQuantities;
    }

    void addSecretIngredient(vector<string> &myList, const vector<string> &friendsList)
    {
        addSecretIngredient(myList, friendsList.back());
    };

    void addSecretIngredient(vector<string> &myList, const string secretIngredient)
    {
        myList.back() = secretIngredient;
    };

} // namespace lasagna_master
