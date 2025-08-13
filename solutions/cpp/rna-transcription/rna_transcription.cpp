#include "rna_transcription.h"
#include <map>

namespace rna_transcription
{
    using namespace std;

    map<char, char> dna_to_rna = {
        {'G', 'C'},
        {'C', 'G'},
        {'T', 'A'},
        {'A', 'U'},
    };

    char to_rna(char nucleotide)
    {
        return dna_to_rna[nucleotide];
    }

    string to_rna(string dna)
    {
        size_t length = dna.size();
        string rna(length, '?');

        for (size_t i = 0; i < length; i++)
        {
            rna[i] = to_rna(dna[i]);
        }
        return rna;
    };
} // namespace rna_transcription
