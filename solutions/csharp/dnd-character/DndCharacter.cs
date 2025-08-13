using System;
using System.Linq;

public class DndCharacter
{
    public int Strength { get; } = Ability();
    public int Dexterity { get; } = Ability();
    public int Constitution { get; } = Ability();
    public int Intelligence { get; } = Ability();
    public int Wisdom { get; } = Ability();
    public int Charisma { get; } = Ability();
    public int Hitpoints { get; }

    private static Random rng = new Random();

    private DndCharacter()
    {
        Hitpoints = 10 + Modifier(Constitution);
    }

    public static int Ability()
    {
        var rolls = Enumerable.Repeat(0, 4).Select(_ => rng.Next(1, 7)).ToArray();
        return rolls.Sum() - rolls.Min();
    }

    public static int Modifier(int score) => score / 2 - 5;

    public static DndCharacter Generate() => new DndCharacter();
}
