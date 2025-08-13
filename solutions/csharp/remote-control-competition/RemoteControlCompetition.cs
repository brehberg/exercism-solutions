using System;
using System.Collections.Generic;

public interface IRemoteControlCar
{
    int DistanceTravelled { get; }
    void Drive();
}

public class ProductionRemoteControlCar : IRemoteControlCar, IComparable<ProductionRemoteControlCar>
{
    public int DistanceTravelled { get; private set; }
    public int NumberOfVictories { get; set; }

    public void Drive() => DistanceTravelled += 10;

    public int CompareTo(ProductionRemoteControlCar other)
    {
        // If other is not a valid object reference, this instance is greater.
        if (other == null) return 1;

        // The Remote Control Car comparison depends on the comparison of
        // the underlying Number Of Victories values.
        return NumberOfVictories.CompareTo(other.NumberOfVictories);
    }
}

public class ExperimentalRemoteControlCar : IRemoteControlCar
{
    public int DistanceTravelled { get; private set; }

    public void Drive() => DistanceTravelled += 20;
}

public static class TestTrack
{
    public static void Race(IRemoteControlCar car) => car.Drive();

    public static List<ProductionRemoteControlCar> GetRankedCars(ProductionRemoteControlCar prc1,
        ProductionRemoteControlCar prc2)
    {
        // Create a list of Production Remote Control Cars and add cars to it.
        var ranking = new List<ProductionRemoteControlCar>() { prc1, prc2 };

        // Call Sort on the list. This will use the default comparer, which is
        // the CompareTo method implemented on Production Remote Control Car.
        ranking.Sort();
        return ranking;
    }
}
