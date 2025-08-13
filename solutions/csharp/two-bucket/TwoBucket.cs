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
        var one = new SingleBucket(Bucket.One, bucketOne, 0);
        var two = new SingleBucket(Bucket.Two, bucketTwo, 0);
        (first, second) = startBucket == Bucket.One ? (one, two) : (two, one);
    }

    public TwoBucketResult Measure(int goal)
    {
        if (!isValid(goal)) throw new ArgumentException("invalid goal amount");
        goalAmount = goal;

        first.fill();
        var moves = 1;

        if (second.size == goalAmount && first.size != goalAmount)
        {
            second.fill();
            moves += 1;
        }
        return solve(moves);
    }

    private TwoBucketResult solve(int moves)
    {
        if (first.amount == goalAmount || second.amount == goalAmount)
        {
            var result = new TwoBucketResult();
            result.Moves = moves;
            result.GoalBucket = first.amount == goalAmount ? first.name : second.name;
            result.OtherBucket = first.amount == goalAmount ? second.amount : first.amount;
            return result;
        }

        if (first.isEmpty()) first.fill();
        else if (second.isFull()) second.empty();
        else first.pour(second);
        return solve(moves + 1);
    }

    private bool isValid(int goal)
    {
        var factor = gcd(first.size, second.size);
        return goal <= Math.Max(first.size, second.size) &&
            (factor == 1 || goal % factor == 0);
    }

    private int gcd(int a, int b)
    {
        if (b == 0) return a;
        return gcd(b, a % b);
    }

    private SingleBucket first;
    private SingleBucket second;
    private int goalAmount;

    private class SingleBucket
    {
        public SingleBucket(Bucket name, int size, int amount)
        {
            this.name = name;
            this.size = size;
            this.amount = amount;
        }

        public bool isFull() => amount == size;
        public bool isEmpty() => amount == 0;
        public void fill() => amount = size;
        public void empty() => amount = 0;
        public void pour(SingleBucket into)
        {
            var quantity = Math.Min(amount, into.size - into.amount);
            amount -= quantity;
            into.amount += quantity;
        }

        public Bucket name { get; }
        public int size { get; }
        public int amount { get; set; }
    }
}
