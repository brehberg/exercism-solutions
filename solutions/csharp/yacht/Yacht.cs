using System;
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
        var groupOfMostDice = dice.GroupBy(die => die).OrderByDescending(g => g.Count()).First();
        return groupOfMostDice.Count() < 4 ? 0 : groupOfMostDice.Key * 4;
    }

    private static int checkFullHouse(int[] dice)
    {
        var groupsOfDice = dice.GroupBy(die => die);
        if (groupsOfDice.Count() != 2) return 0;
        var groupOfMostDice = groupsOfDice.OrderByDescending(g => g.Count()).First();
        return groupOfMostDice.Count() == 3 ? dice.Sum() : 0;
    }

    private static int checkStraight(int[] dice, int start)
    {
        Array.Sort(dice);
        foreach (var die in dice)
        {
            if (die != start++) return 0;
        }
        return 30; // a straight is worth 30 points if all required dice values are present
    }

    private static int sumForValue(int[] dice, int value) =>
        dice.Where(die => die == value).Sum();
}

