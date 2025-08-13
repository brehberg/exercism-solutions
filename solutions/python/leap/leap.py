def leap_year(year):
    """Verify that a given year is a leap year.

    :param year: int - the year to check
    :return: bool - this is a leap year?
    """

    def divisible_by(n):
        return year % n == 0

    # Occurs on every year that is evenly divisible by 4
    # except every year that is evenly divisible by 100
    # unless the year is also evenly divisible by 400.
    return divisible_by(4) and not divisible_by(100) or divisible_by(400)
