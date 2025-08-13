#include <string>
#include <vector>

namespace election
{
    struct ElectionResult
    {
        std::string name{}; // Name of the candidate
        int votes{};        // Number of votes the candidate has
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
    ElectionResult &determine_result(std::vector<ElectionResult> &results)
    {
        int winning{0};
        for (size_t i = 0; i < results.size(); i++)
        {
            if (results[i].votes > results[winning].votes)
            {
                winning = i;
            }
        }
        ElectionResult &winner{results[winning]};
        winner.name = "President " + winner.name;
        return winner;
    }
} // namespace election