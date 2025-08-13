module TwoBucket
  enum Bucket
    One
    Two
  end

  struct Result
    property moves, other_bucket, goal_bucket

    def initialize(@moves : UInt32, @other_bucket : UInt32, @goal_bucket : Bucket)
    end
  end

  def self.measure(bucket_one : UInt32, bucket_two : UInt32, goal : UInt32, start_bucket : Bucket)
    raise ArgumentError.new if !valid?(bucket_one, bucket_two, goal)

    one = SingleBucket.new(Bucket::One, bucket_one)
    two = SingleBucket.new(Bucket::Two, bucket_two)
    if start_bucket == Bucket::One
      first, other = one, two
    else
      first, other = two, one
    end

    if other.size == goal && first.size != goal
      # simply fill both buckets to reach the goal
      return Result.new(2, first.size, other.name)
    end

    moves : UInt32 = 0
    until first.amount == goal || other.amount == goal
      if first.empty?
        first.fill
      elsif other.full?
        other.empty
      else
        first.pour(into: other)
      end
      moves += 1
    end

    first.amount == goal ? Result.new(moves, other.amount, first.name) : Result.new(moves, first.amount, other.name)
  end

  def self.valid?(bucket_one : UInt32, bucket_two : UInt32, goal : UInt32)
    goal <= Math.max(bucket_one, bucket_two) && goal % bucket_one.gcd(bucket_two) == 0
  end

  # single bucket class type and helper methods
  class SingleBucket
    property name, size, amount : UInt32

    def initialize(@name : Bucket, @size : UInt32)
      @amount = 0
    end

    def full?
      @amount == size
    end

    def empty?
      @amount == 0
    end

    def fill
      @amount = size
    end

    def empty
      @amount = 0
    end

    def pour(into : SingleBucket)
      quantity : UInt32 = Math.min(@amount, into.size - into.amount)
      @amount -= quantity
      into.amount += quantity
    end
  end
end
