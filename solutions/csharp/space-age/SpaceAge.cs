using System;
using System.Collections.Generic;

public class SpaceAge
{
    private const double earthYearInSeconds = 31557600.0;
    private enum Planet { Mercury, Venus, Earth, Mars, Jupiter, Saturn, Uranus, Neptune }
    private readonly Dictionary<Planet, double> orbitalPeriod = new Dictionary<Planet, double>()
    {
        {Planet.Mercury, 0.2408467},
        {Planet.Venus,   0.61519726},
        {Planet.Earth,   1.0},
        {Planet.Mars,    1.8808158},
        {Planet.Jupiter, 11.862615},
        {Planet.Saturn,  29.447498},
        {Planet.Uranus,  84.016846},
        {Planet.Neptune, 164.79132},
    };

    private readonly double yearsOnEarth;

    public SpaceAge(int seconds) => yearsOnEarth = seconds / earthYearInSeconds;

    public double OnEarth() => yearsOnEarth / orbitalPeriod[Planet.Earth];
    public double OnMercury() => yearsOnEarth / orbitalPeriod[Planet.Mercury];
    public double OnVenus() => yearsOnEarth / orbitalPeriod[Planet.Venus];
    public double OnMars() => yearsOnEarth / orbitalPeriod[Planet.Mars];
    public double OnJupiter() => yearsOnEarth / orbitalPeriod[Planet.Jupiter];
    public double OnSaturn() => yearsOnEarth / orbitalPeriod[Planet.Saturn];
    public double OnUranus() => yearsOnEarth / orbitalPeriod[Planet.Uranus];
    public double OnNeptune() => yearsOnEarth / orbitalPeriod[Planet.Neptune];
}