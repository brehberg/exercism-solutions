"""Python solution for Raindrops on Exercism."""
FACTORS = ("Pling", 3), ("Plang", 5), ("Plong", 7)


def convert(num):
    """
    Convert returns a string that contains raindrop sounds corresponding to potential factors.

    :param num: int - given number to convert
    :return: string - corresponding raindrop sound

    The rules of raindrops are that if a given number:
        has 3 as a factor, add 'Pling' to the result.
        has 5 as a factor, add 'Plang' to the result.
        has 7 as a factor, add 'Plong' to the result.
        does not have any of 3, 5, or 7 as a factor, result should be the digits of the number.
    """
    return "".join(f"{drop}" for drop, val in FACTORS if not num % val) or str(num)
