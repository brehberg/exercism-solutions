using System;
using System.Linq;

public class DndCharacter
{
    public int Strength { get; }
    public int Dexterity { get; }
    public int Constitution { get; }
    public int Intelligence { get; }
    public int Wisdom { get; }
    public int Charisma { get; }
    public int Hitpoints { get; }

    private static Random rng = new Random();

    private DndCharacter()
    {
        Strength = Ability();
        Dexterity = Ability();
        Constitution = Ability();
        Intelligence = Ability();
        Wisdom = Ability();
        Charisma = Ability();
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
