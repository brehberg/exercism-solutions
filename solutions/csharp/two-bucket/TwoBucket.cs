using System;

public enum Bucket
{
    One,
    Two
}

public class TwoBucketResult
{
    public int Moves { get; set; }
    public Bucket GoalBucket { get; set; }
    public int OtherBucket { get; set; }
}

public class TwoBucket
{
    public TwoBucket(int bucketOne, int bucketTwo, Bucket startBucket)
    {
        var one = new SingleBucket(Bucket.One, bucketOne);
        var two = new SingleBucket(Bucket.Two, bucketTwo);
        (first, second) = startBucket == Bucket.One ? (one, two) : (two, one);
    }

    public TwoBucketResult Measure(int goal)
    {
        if (!isValid(goal)) throw new ArgumentException("invalid goal amount");

        first.Fill();
        var moves = 1;

        if (second.Size == goal && first.Size != goal)
        {
            second.Fill();
            moves += 1;
        }
        while (first.Amount != goal && second.Amount != goal)
        {
            if (first.IsEmpty()) first.Fill();
            else if (second.IsFull()) second.Empty();
            else first.Pour(second);
            moves += 1;
        }

        var result = new TwoBucketResult();
        result.Moves = moves;
        result.GoalBucket = first.Amount == goal ? first.Name : second.Name;
        result.OtherBucket = first.Amount == goal ? second.Amount : first.Amount;
        return result;
    }

    private bool isValid(int goal)
    {
        // bool isDivisibleBy(int n) => year % n == 0;
        var factor = gcd(first.Size, second.Size);
        return goal <= Math.Max(first.Size, second.Size) &&
            (factor == 1 || goal % factor == 0);
    }

    private int gcd(int a, int b) => b == 0 ? a : gcd(b, a % b);

    private SingleBucket first;
    private SingleBucket second;

    private class SingleBucket(Bucket name, int size)
    {
        public bool IsFull() => Amount == Size;
        public bool IsEmpty() => Amount == 0;
        public void Fill() => Amount = Size;
        public void Empty() => Amount = 0;
        public void Pour(SingleBucket into)
        {
            var quantity = Math.Min(Amount, into.Size - into.Amount);
            Amount -= quantity;
            into.Amount += quantity;
        }

        public Bucket Name => name;
        public int Size => size;
        public int Amount { get; set; }
    }
}
