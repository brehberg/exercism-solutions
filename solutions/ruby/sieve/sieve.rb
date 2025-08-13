class Sieve
  def initialize(limit)
    @limit = limit
  end

  def primes
    candidates = [nil, nil, *2..@limit]
    (2..Math.sqrt(@limit)).each do |n|
      next unless candidates[n]
      (n * n..@limit).step(n) { |m| candidates[m] = nil }
    end
    candidates.compact
  end
end
