using System;

public static class RotationalCipher
{
    public static string Rotate(string plaintext, int shiftKey)
    {
        char rotateChar(char c, char start) =>
            (char)(start + (c + shiftKey - start) % 26);

        char[] cyphertext = plaintext.ToCharArray();
        for (int i = 0; i < cyphertext.Length; ++i)
        {
            char c = cyphertext[i];
            if (char.IsLetter(c))
                cyphertext[i] = rotateChar(c, char.IsLower(c) ? 'a' : 'A');
        }
        return new string(cyphertext);
    }
}