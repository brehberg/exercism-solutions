using System;
using System.Text;
using System.Collections.Generic;

public static class RnaTranscription
{
    private static readonly Dictionary<char, char> dnaToRna = new Dictionary<char, char>
    {
        {'G', 'C'},
        {'C', 'G'},
        {'T', 'A'},
        {'A', 'U'},
    };
    public static string ToRna(string dna)
    {
        StringBuilder sb = new StringBuilder(dna.Length);
        foreach (var nucleotide in dna)
        {
            sb.Append(dnaToRna[nucleotide]);
        }
        return sb.ToString();
    }
}