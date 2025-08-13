// @ts-check

/**
 * Sieve of Eratosthenes algorithm for finding all prime numbers up to the given limit.
 *
 * @param {Number} limit
 * @returns {Number[]} prime numbers.
 */
export const primes = (limit) => {
  if (limit < 2) {
    return [];
  }

  const multiples = new Set([0, 1]);
  for (let n = 2; n < Math.sqrt(limit); n += 1) {
    for (let m = n * n; m <= limit; m += n) {
      multiples.add(m);
    }
  }

  const primes = new Set();
  [...Array(limit + 1).keys()].forEach((number) => {
    if (!multiples.has(number)) {
      primes.add(number);
    }
  });

  return Array.from(primes);
};
