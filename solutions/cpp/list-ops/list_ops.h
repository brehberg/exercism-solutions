#pragma once
#include <vector>

namespace list_ops
{
    void append(std::vector<int> &, std::vector<int>);
    void append(std::vector<std::vector<int>> &, std::vector<std::vector<int>>);
    std::vector<int> concat(std::vector<std::vector<int>>);
    std::vector<std::vector<int>> concat(std::vector<std::vector<std::vector<int>>>);

    std::vector<int> filter(std::vector<int>, bool (*)(int));
    std::size_t length(std::vector<int>);
    std::vector<int> map(std::vector<int>, int (*)(int));

    int foldl(std::vector<int>, int, int (*)(int, int));
    int foldr(std::vector<int>, int, int (*)(int, int));
    double foldl(std::vector<double>, double, double (*)(double, double));
    double foldr(std::vector<double>, double, double (*)(double, double));

    std::vector<int> reverse(std::vector<int>);
    std::vector<double> reverse(std::vector<double>);
    std::vector<std::vector<int>> reverse(std::vector<std::vector<int>>);

} // namespace list_ops
