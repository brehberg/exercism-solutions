def steps(num: int) -> int:
    """Collatz Conjecture"""
    if num < 1:
        raise ValueError("Only positive integers are allowed")
    count: int = 0
    while num > 1:
        count += 1
        num = 3 * num + 1 if num % 2 else num // 2
    return count
