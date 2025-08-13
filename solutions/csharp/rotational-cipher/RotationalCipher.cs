using System;

public static class RotationalCipher
{
    public static string Rotate(string plaintext, int shiftKey)
    {
        char[] cyphertext = plaintext.ToCharArray();
        for (int i = 0; i < cyphertext.Length; ++i)
        {
            char c = cyphertext[i];
            if ('a' <= c && c <= 'z')
            {
                cyphertext[i] = rotateChar(c, (char)shiftKey, 'a');
            }
            else if ('A' <= c && c <= 'Z')
            {
                cyphertext[i] = rotateChar(c, (char)shiftKey, 'A');
            }
        }
        return new string(cyphertext);
    }

    private static char rotateChar(char c, char n, char start) =>
        (char)(start + (c + n - start) % 26);
}