#pragma once

#include <string>
#include <vector>

namespace lasagna_master
{
    struct amount
    {
        int noodles;
        double sauce;
    };

    const int gramsOfNoodlesPerLayer{50};
    const double litersOfSaucePerLayer{0.2};
    const int standardRecipePortions{2};

    // 1. Estimate the preparation time
    int preparationTime(const std::vector<std::string> &layers, int timePerLayer = 2);
    // 2. Compute the amounts of noodles and sauce needed
    amount quantities(const std::vector<std::string> &layers);
    // 3. Add the secret ingredient
    void addSecretIngredient(std::vector<std::string> &myList, const std::vector<std::string> &friendsList);
    // 4. Scale the recipe
    std::vector<double> scaleRecipe(const std::vector<double> &quantities, int portions);
    // 5. Unlock the Family Secret
    void addSecretIngredient(std::vector<std::string> &myList, const std::string secretIngredient);

} // namespace lasagna_master
