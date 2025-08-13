package sieve

// Sieve of Eratosthenes algorithm for finding all prime numbers up to the given limit.
func Sieve(limit int) (primes []int) {
	if limit < 2 {
		return
	}

	list := make([]bool, limit+1)
	list[0] = true
	list[1] = true

	for i, marked := range list {
		if !marked {
			for j := i * i; j <= limit; j += i {
				list[j] = true
			}
			primes = append(primes, i)
		}
	}
	return
}
