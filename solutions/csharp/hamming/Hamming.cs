using System;

public static class Hamming
{
    public static int Distance(string firstStrand, string secondStrand)
    {
        if (firstStrand.Length != secondStrand.Length)
            throw new ArgumentException("Strands have different lengths.");

        // Calculate the Hamming Distance between two DNA strands.
        int differenceCount = 0;
        for (int i = 0; i < firstStrand.Length; i++)
        {
            if (firstStrand[i] != secondStrand[i]) differenceCount += 1;
        }
        return differenceCount;
    }
}