module main
import math

enum BucketId {
	one
	two
}

struct Solution {
	moves        int
	goal_bucket  BucketId
	other_bucket int
}

pub fn measure(capacity_one int, capacity_two int, goal int, start_bucket BucketId) !Solution {
	if !is_valid(capacity_one, capacity_two, goal) {
		return error("impossible")
	}

	one := new_bucket(BucketId.one, capacity_one)
	two := new_bucket(BucketId.two, capacity_two)
	mut first, mut other := match start_bucket {
		.one { one, two }
		.two { two, one }
	}

    if other.size == goal && first.size != goal {
		// simply fill both buckets to reach the goal
		return Solution{
			moves: 2
			goal_bucket: other.name
			other_bucket: first.size
		}
	}

	mut moves := 0
	for first.amount != goal && other.amount != goal {
		match true {
			first.is_empty() { first.fill() }
			other.is_full() { other.empty() }
			else { first.pour(mut other) }
		}
		moves += 1
	}

	return Solution{
		moves: moves
		goal_bucket: if first.amount == goal {first.name} else {other.name}
		other_bucket: if first.amount == goal {other.amount} else {first.amount}
	}
}

fn is_valid(bucket_one int, bucket_two int, goal int) bool {
	return goal <= math.max(bucket_one, bucket_two) && 
		goal % math.gcd(bucket_one, bucket_two) == 0
}


// single bucket type and helper functions
struct Bucket {
	name   BucketId @[required]
	size   int      @[required]
mut:
	amount int
}
fn new_bucket(name BucketId, size int) Bucket {
	return Bucket{
		name: name
		size: size
		amount: 0
	}
}
fn (b Bucket) is_full() bool {
	return b.amount == b.size
}
fn (b Bucket) is_empty() bool {
	return b.amount == 0
}
fn (mut b Bucket) fill() {
	b.amount = b.size
}
fn (mut b Bucket) empty() {
	b.amount = 0
}
fn (mut b Bucket) pour(mut into Bucket) {
	quantity := math.min(b.amount, into.size - into.amount)
	b.amount -= quantity
	into.amount += quantity
}
