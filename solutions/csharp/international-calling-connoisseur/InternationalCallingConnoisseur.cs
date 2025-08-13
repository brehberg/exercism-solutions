using System;
using System.Collections.Generic;

public static class DialingCodes
{
    // Create a new dictionary
    public static Dictionary<int, string> GetEmptyDictionary() => new Dictionary<int, string>();

    // Create a pre-populated dictionary
    public static Dictionary<int, string> GetExistingDictionary() => new Dictionary<int, string>
    {
        {1, "United States of America"},
        {55, "Brazil"},
        {91, "India"},
    };

    // Add a country to an empty dictionary
    public static Dictionary<int, string> AddCountryToEmptyDictionary(int countryCode, string countryName)
    {
        var codeDictionary = GetEmptyDictionary();
        codeDictionary.Add(countryCode, countryName);
        return codeDictionary;
    }

    // Add a country to an existing dictionary
    public static Dictionary<int, string> AddCountryToExistingDictionary(
        Dictionary<int, string> existingDictionary, int countryCode, string countryName)
    {
        existingDictionary.Add(countryCode, countryName);
        return existingDictionary;
    }

    // Get the country name matching a dialing code
    public static string GetCountryNameFromDictionary(
        Dictionary<int, string> existingDictionary, int countryCode) =>
            existingDictionary.GetValueOrDefault(countryCode, "");

    // Check that a country exists in the dictionary
    public static bool CheckCodeExists(Dictionary<int, string> existingDictionary, int countryCode) =>
        existingDictionary.ContainsKey(countryCode);

    // Update a country name
    public static Dictionary<int, string> UpdateDictionary(
        Dictionary<int, string> existingDictionary, int countryCode, string countryName)
    {
        if (CheckCodeExists(existingDictionary, countryCode))
        {
            existingDictionary[countryCode] = countryName;
        }
        return existingDictionary;
    }

    // Remove a country from the dictionary
    public static Dictionary<int, string> RemoveCountryFromDictionary(
        Dictionary<int, string> existingDictionary, int countryCode)
    {
        existingDictionary.Remove(countryCode);
        return existingDictionary;
    }

    // Find the country with the longest name
    public static string FindLongestCountryName(Dictionary<int, string> existingDictionary)
    {
        var longestName = "";
        foreach (var countryName in existingDictionary.Values)
        {
            if (countryName.Length > longestName.Length)
            {
                longestName = countryName;
            }
        }
        return longestName;
    }
}