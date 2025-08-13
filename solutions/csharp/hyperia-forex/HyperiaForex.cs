using System;

public struct CurrencyAmount
{
    private decimal amount;
    private string currency;

    public CurrencyAmount(decimal amount, string currency)
    {
        this.amount = amount;
        this.currency = currency;
    }

    // 1. Enable the currency amount to be tested for equality
    public static bool operator ==(CurrencyAmount a, CurrencyAmount b)
        => a.currency == b.currency
            ? a.amount == b.amount
            : throw new ArgumentException("different currency");
    public static bool operator !=(CurrencyAmount a, CurrencyAmount b)
        => !(a == b);
    public override int GetHashCode() => (amount, currency).GetHashCode();
    public override bool Equals(object obj)
        => obj is null || this.GetType() != obj.GetType() ? false
            : this == (CurrencyAmount)obj;

    // 2. Compare currency amounts
    public static bool operator >(CurrencyAmount a, CurrencyAmount b)
        => a != b && a.amount > b.amount;
    public static bool operator <(CurrencyAmount a, CurrencyAmount b)
        => a != b && a.amount < b.amount;
    public static bool operator >=(CurrencyAmount a, CurrencyAmount b)
        => a == b || a > b;
    public static bool operator <=(CurrencyAmount a, CurrencyAmount b)
        => a == b || a < b;

    // 3. Add and subtract currency amounts
    public static CurrencyAmount operator +(CurrencyAmount a) => a;
    public static CurrencyAmount operator +(CurrencyAmount a, CurrencyAmount b)
        => a.currency == b.currency
            ? new CurrencyAmount(a.amount + b.amount, a.currency)
            : throw new ArgumentException("different currency");
    public static CurrencyAmount operator -(CurrencyAmount c)
        => new CurrencyAmount(-c.amount, c.currency);
    public static CurrencyAmount operator -(CurrencyAmount a, CurrencyAmount b)
        => a + (-b);

    // 4. Multiply and divide currency amounts
    public static CurrencyAmount operator *(CurrencyAmount c, decimal d)
        => new CurrencyAmount(c.amount * d, c.currency);
    public static CurrencyAmount operator *(decimal d, CurrencyAmount c)
        => c * d;
    public static CurrencyAmount operator /(CurrencyAmount c, decimal d)
        => new CurrencyAmount(c.amount / d, c.currency);

    // 5. Convert the currency amount to a double
    public static explicit operator double(CurrencyAmount a)
        => (double)a.amount;

    // 6. Convert the currency amount to a decimal
    public static implicit operator decimal(CurrencyAmount a)
        => a.amount;
}
