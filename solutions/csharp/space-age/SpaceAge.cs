using System;
using System.Collections.Generic;

public class SpaceAge
{
    private const double earthYearInSeconds = 31557600.0;
    private readonly Dictionary<string, double> orbitalPeriod = new Dictionary<string, double>()
    {
        {"Mercury", 0.2408467},
        {"Venus",   0.61519726},
        {"Earth",   1.0},
        {"Mars",    1.8808158},
        {"Jupiter", 11.862615},
        {"Saturn",  29.447498},
        {"Uranus",  84.016846},
        {"Neptune", 164.79132},
    };

    private readonly double yearsOnEarth;

    public SpaceAge(int seconds) => yearsOnEarth = seconds / earthYearInSeconds;

    public double OnEarth() => yearsOnEarth / orbitalPeriod["Earth"];
    public double OnMercury() => yearsOnEarth / orbitalPeriod["Mercury"];
    public double OnVenus() => yearsOnEarth / orbitalPeriod["Venus"];
    public double OnMars() => yearsOnEarth / orbitalPeriod["Mars"];
    public double OnJupiter() => yearsOnEarth / orbitalPeriod["Jupiter"];
    public double OnSaturn() => yearsOnEarth / orbitalPeriod["Saturn"];
    public double OnUranus() => yearsOnEarth / orbitalPeriod["Uranus"];
    public double OnNeptune() => yearsOnEarth / orbitalPeriod["Neptune"];
}