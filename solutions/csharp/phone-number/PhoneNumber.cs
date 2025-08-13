using System;
using System.Linq;

public class PhoneNumber
{
    public static string Clean(string phoneNumber)
    {
        // extract all digits from the given string
        var digits = phoneNumber.Where(Char.IsDigit).ToArray();

        // all NANP-numbers share the same country code
        if (digits.Length == 11 && digits[0] == '1')
        {
            digits = digits.Skip(1).ToArray();
        }

        // area and exchange codes only digits from 2 through 9
        if (digits.Length != 10
            || digits[0] is '0' or '1'
            || digits[3] is '0' or '1')
        {
            throw new ArgumentException("invalid phone number");
        }

        return new string(digits);
    }
}