using System;

public class Player
{
    private Random _rng = new Random();

    public int RollDie() => _rng.Next(1, 18);

    public double GenerateSpellStrength() => _rng.NextDouble() * 100;
}
