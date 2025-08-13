"""Function to find prime numbers up to the given limit."""


def primes(limit):
    """Sieve of Eratosthenes algorithm for finding all prime numbers.

    :param limit: int - given limit to find primes less than.
    :return: list - sorted list of prime numbers.
    """
    numbers = set(range(2, limit + 1))
    marked = {m for n in numbers for m in range(n * n, limit + 1, n)}
    return sorted(numbers - marked)
