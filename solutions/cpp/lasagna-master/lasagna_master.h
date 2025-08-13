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

    int preparationTime(const std::vector<std::string> &layers, int timePerLayer = 2);

    amount quantities(const std::vector<std::string> &layers);

    void addSecretIngredient(std::vector<std::string> &myList, const std::vector<std::string> &friendsList);

    std::vector<double> scaleRecipe(const std::vector<double> &quantities, int portions);

    void addSecretIngredient(std::vector<std::string> &myList, const std::string secretIngredient);

} // namespace lasagna_master
