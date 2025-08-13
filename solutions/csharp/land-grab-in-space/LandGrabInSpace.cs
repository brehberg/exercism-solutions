using System;
using System.Collections.Generic;
using System.Linq;

public struct Coord
{
    public Coord(ushort x, ushort y)
    {
        X = x;
        Y = y;
    }
    public ushort X { get; }
    public ushort Y { get; }
}
public struct Side
{
    public Side(Coord a, Coord b)
    {
        A = a;
        B = b;
    }
    public Coord A { get; }
    public Coord B { get; }
    public float Length() => (B.X - A.X) * 2 + (B.Y - A.Y) * 2;
}
public struct Plot
{
    public Plot(Coord w, Coord x, Coord y, Coord z)
    {
        Coordinates = (w, x, y, z);

        Side[] sides = new Side[4] {
            new Side(w, x),
            new Side(x, y),
            new Side(y, z),
            new Side(z, w)};

        LongestSide = sides.OrderByDescending((s => s.Length())).First();
    }

    public (Coord, Coord, Coord, Coord) Coordinates { get; }
    public Side LongestSide { get; }

    public bool hasLongerSide(Plot plot) =>
        this.LongestSide.Length() > plot.LongestSide.Length();
}


public class ClaimsHandler
{
    private HashSet<Plot> claims = new HashSet<Plot>();
    private Plot lastClaimStaked;
    private Plot claimWithLongestSide;

    public void StakeClaim(Plot plot)
    {
        claims.Add(plot);
        lastClaimStaked = plot;
        if (plot.hasLongerSide(claimWithLongestSide))
            claimWithLongestSide = plot;
    }

    public bool IsClaimStaked(Plot plot) => claims.Contains(plot);

    public bool IsLastClaim(Plot plot) => lastClaimStaked.Equals(plot);

    public Plot GetClaimWithLongestSide() => claimWithLongestSide;
}
