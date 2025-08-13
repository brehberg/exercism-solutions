"""Function to find prime numbers up to the given limit."""


def primes(limit):
    """Sieve of Eratosthenes algorithm for finding all prime numbers.

    :param limit: int - given limit to find primes less than.
    :return: list - sorted list of prime numbers.
    """
    if limit < 2:
        return []
    numbers = set(range(3, limit + 1, 2))
    multiples = {m for n in numbers for m in range(n * n, limit + 1, 2 * n)}
    return [2] + sorted(numbers - multiples)
