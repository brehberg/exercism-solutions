#include <string>
#include <vector>
#include <algorithm>

using namespace std;

namespace election
{
    struct ElectionResult
    {
        string name{}; // Name of the candidate
        int votes{};   // Number of votes the candidate has
    };

    // Get the number of votes from an ElectionResult
    int vote_count(ElectionResult &result)
    {
        return result.votes;
    }

    // Increment the votes of an ElectionResult
    void increment_vote_count(ElectionResult &result, int votes)
    {
        result.votes += votes;
    }

    // Vote counting and Presidency
    ElectionResult &determine_result(vector<ElectionResult> &results)
    {
        auto &winner = *max_element(results.begin(), results.end(),
                                    [](auto &a, auto &b)
                                    { return a.votes < b.votes; });
        winner.name = "President " + winner.name;
        return winner;
    }
} // namespace election