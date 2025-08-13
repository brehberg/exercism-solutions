"""Python solution for Roman Numerals on Exercism."""

LITERALS = [
    (1000, "M"),
    (500, "D"),
    (100, "C"),
    (50, "L"),
    (10, "X"),
    (5, "V"),
    (1, "I"),
]


def roman(number):
    """Given a number, convert it into a roman numeral.

    :param number: int - the number to turn into roman numeral.
    :return: str - representation of that number in roman numeral form.
    """
    for value, literal in LITERALS:
        if value in [100, 10, 1]:
            if int(number / value) in [9, 4]:
                return literal + roman(number + value)

    for value, literal in LITERALS:
        if number >= value:
            return literal + roman(number - value)

    return ""
