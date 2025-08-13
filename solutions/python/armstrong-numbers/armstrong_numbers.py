def is_armstrong_number(number: int) -> bool:
    """Returns true if the given number is an Armstrong Number.
    An Armstrong number is a number that is the sum of its own
    digits each raised to the power of the number of digits.
    """
    digits = str(number)
    digit_sum = 0
    for c in digits:
        digit_sum += int(c) ** len(digits)
    return digit_sum == number
