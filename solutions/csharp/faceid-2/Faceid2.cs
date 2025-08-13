using System;
using System.Collections.Generic;

public class FacialFeatures : IEquatable<FacialFeatures>
{
    public string EyeColor { get; }
    public decimal PhiltrumWidth { get; }

    public FacialFeatures(string eyeColor, decimal philtrumWidth)
    {
        EyeColor = eyeColor;
        PhiltrumWidth = philtrumWidth;
    }

    public override int GetHashCode() => (EyeColor, PhiltrumWidth).GetHashCode();
    public override bool Equals(object obj) => this.Equals(obj as FacialFeatures);

    public bool Equals(FacialFeatures other)
    {
        if (other is null) return false;
        // Optimization for a common success case.
        if (Object.ReferenceEquals(this, other)) return true;
        // If run-time types are not exactly the same, return false.
        if (this.GetType() != other.GetType()) return false;
        // Return true if the fields match.
        return (EyeColor == other.EyeColor)
            && (PhiltrumWidth == other.PhiltrumWidth);
    }
}

public class Identity : IEquatable<Identity>
{
    public string Email { get; }
    public FacialFeatures FacialFeatures { get; }

    public Identity(string email, FacialFeatures facialFeatures)
    {
        Email = email;
        FacialFeatures = facialFeatures;
    }

    public override int GetHashCode() => (
            Email,
            FacialFeatures.EyeColor,
            FacialFeatures.PhiltrumWidth
        ).GetHashCode();
    public override bool Equals(object obj) => this.Equals(obj as Identity);

    public bool Equals(Identity other)
    {
        if (other is null) return false;
        if (Object.ReferenceEquals(this, other)) return true;
        if (this.GetType() != other.GetType()) return false;
        return (Email == other.Email)
            && (FacialFeatures.Equals(other.FacialFeatures));
    }
}

public class Authenticator
{
    private Identity admin = new Identity("admin@exerc.ism", new FacialFeatures("green", 0.9m));
    private HashSet<Identity> registeredIdentities = new HashSet<Identity> { };

    // 1. Ensure that facial features match
    public static bool AreSameFace(FacialFeatures faceA, FacialFeatures faceB) => faceA.Equals(faceB);

    // 2. Authenticate the system administrator
    public bool IsAdmin(Identity identity) => admin.Equals(identity);

    // 3. Register new identities
    public bool Register(Identity identity) => registeredIdentities.Add(identity);

    // 4. Prevent invalid identities being authenticated
    public bool IsRegistered(Identity identity) => registeredIdentities.Contains(identity);

    // 5. Add diagnostics to detect multiple attempts to authenticate
    public static bool AreSameObject(Identity identityA, Identity identityB) => identityA == identityB;
}
