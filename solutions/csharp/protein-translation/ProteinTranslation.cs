using System;
using System.Collections.Generic;

public static class ProteinTranslation
{
    private static string StopCodon = "STOP";

    public static string[] Proteins(string strand)
    {
        List<string> result = new List<string>();
        for (int i = 0; i < strand.Length; i += 3)
        {
            string protein = FromCodon(strand.Substring(i, 3));
            if (protein == StopCodon) break;
            result.Add(protein);
        }
        return result.ToArray();
    }

    private static string FromCodon(string codon) => codon switch
    {
        "AUG" => "Methionine",
        "UUU" or "UUC" => "Phenylalanine",
        "UUA" or "UUG" => "Leucine",
        "UCU" or "UCC" or "UCA" or "UCG" => "Serine",
        "UAU" or "UAC" => "Tyrosine",
        "UGU" or "UGC" => "Cysteine",
        "UGG" => "Tryptophan",
        "UAA" or "UAG" or "UGA" => StopCodon,
        _ => "",
    };

}