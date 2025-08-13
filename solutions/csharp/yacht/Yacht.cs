using System;
using System.Collections.Generic;
using System.Linq;

public enum YachtCategory
{
    Ones = 1,
    Twos = 2,
    Threes = 3,
    Fours = 4,
    Fives = 5,
    Sixes = 6,
    FullHouse = 7,
    FourOfAKind = 8,
    LittleStraight = 9,
    BigStraight = 10,
    Choice = 11,
    Yacht = 12,
}

public static class YachtGame
{
    public static int Score(int[] dice, YachtCategory category) => category switch
    {
        YachtCategory.Ones => sumForValue(dice, 1),
        YachtCategory.Twos => sumForValue(dice, 2),
        YachtCategory.Threes => sumForValue(dice, 3),
        YachtCategory.Fours => sumForValue(dice, 4),
        YachtCategory.Fives => sumForValue(dice, 5),
        YachtCategory.Sixes => sumForValue(dice, 6),

        YachtCategory.FullHouse => checkFullHouse(dice),
        YachtCategory.FourOfAKind => checkFourOfAKind(dice),
        YachtCategory.LittleStraight => checkStraight(dice, 1),
        YachtCategory.BigStraight => checkStraight(dice, 2),
        YachtCategory.Yacht => checkYacht(dice),
        YachtCategory.Choice => dice.Sum(),

        _ => 0,
    };

    private static int checkYacht(int[] dice) =>
        dice.Distinct().Count() == 1 ? 50 : 0;

    private static int checkFourOfAKind(int[] dice)
    {
        var groupOfMostDice = dice.GroupBy(die => die).largestGroup();
        return groupOfMostDice.Count() >= 4 ? groupOfMostDice.Key * 4 : 0;
    }

    private static int checkFullHouse(int[] dice)
    {
        var groupsOfDice = dice.GroupBy(die => die);
        if (groupsOfDice.Count() != 2) return 0;
        return groupsOfDice.largestGroup().Count() == 3 ? dice.Sum() : 0;
    }

    private static int checkStraight(int[] dice, int start) =>
        dice.OrderBy(die => die).SequenceEqual(Enumerable.Range(start, 5)) ? 30 : 0;

    private static int sumForValue(int[] dice, int value) =>
        dice.Where(die => die == value).Sum();

    private static IGrouping<T, T> largestGroup<T>(this IEnumerable<IGrouping<T, T>> groups) =>
        groups.OrderByDescending(g => g.Count()).First();
}

