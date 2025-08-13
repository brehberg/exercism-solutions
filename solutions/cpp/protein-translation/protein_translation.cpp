#include "protein_translation.h"
#include <string>
#include <vector>
#include <map>

using namespace std;

namespace protein_translation
{
    static string STOP_CODON = "STOP";

    static map<string, string> MAPPING = {
        {"UGU", "Cysteine"},
        {"UGC", "Cysteine"},
        {"UUA", "Leucine"},
        {"UUG", "Leucine"},
        {"AUG", "Methionine"},
        {"UUU", "Phenylalanine"},
        {"UUC", "Phenylalanine"},
        {"UCU", "Serine"},
        {"UCC", "Serine"},
        {"UCA", "Serine"},
        {"UCG", "Serine"},
        {"UGG", "Tryptophan"},
        {"UAU", "Tyrosine"},
        {"UAC", "Tyrosine"},
        {"UAA", STOP_CODON},
        {"UAG", STOP_CODON},
        {"UGA", STOP_CODON}};

    vector<string> proteins(string strand)
    {
        vector<string> result;
        for (size_t i = 0; i < strand.size(); i += 3)
        {
            string amino_acid = MAPPING.at(strand.substr(i, 3));
            if (amino_acid == STOP_CODON)
            {
                break;
            }
            result.emplace_back(amino_acid);
        }
        return result;
    }
} // namespace protein_translation
