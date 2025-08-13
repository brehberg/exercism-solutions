#include "acronym.h"

#include <regex>
#include <algorithm>

namespace acronym
{
    using namespace std;

    string acronym(string phrase)
    {
        string clean = regex_replace(phrase, regex(R"([_-])"), " ");
        string acronym = regex_replace(clean, regex(R"(\B[\w']+|[\s\W])"), "");
        transform(acronym.begin(), acronym.end(), acronym.begin(), ::toupper);
        return acronym;
    }
} // namespace acronym
