"""Functions to automate Conda airlines ticketing system."""

SEAT_LETTERS = ["A", "B", "C", "D"]


def generate_seat_letters(number):
    """Generate a series of letters for airline seats.

    :param number: int - total number of seat letters to be generated.
    :return: generator - generator that yields seat letters.

    Seat letters are generated from A to D.
    After D it should start again with A.

    Example: A, B, C, D

    """
    next_seat = 0
    while next_seat < number:
        yield SEAT_LETTERS[next_seat % len(SEAT_LETTERS)]
        next_seat += 1


def generate_seats(number):
    """Generate a series of identifiers for airline seats.

    :param number: int - total number of seats to be generated.
    :return: generator - generator that yields seat numbers.

    A seat number consists of the row number and the seat letter.

    There is no row 13.
    Each row has 4 seats.

    Seats should be sorted from low to high.

    Example: 3C, 3D, 4A, 4B

    """
    next_seat = 0
    current_row = 1
    seat_letters = generate_seat_letters(number)

    while next_seat < number:
        seat_letter = next(seat_letters, "")
        yield str(current_row) + seat_letter

        current_row += seat_letter == SEAT_LETTERS[-1]
        current_row += current_row == 13
        next_seat += 1


def assign_seats(passengers):
    """Assign seats to passengers.

    :param passengers: list[str] - a list of strings containing names of passengers.
    :return: dict - with the names of the passengers as keys and seat numbers as values.

    Example output: {"Adele": "1A", "Björk": "1B"}

    """

    seats = generate_seats(len(passengers))
    return {passenger: next(seats) for passenger in passengers}


def generate_codes(seat_numbers, flight_id):
    """Generate codes for a ticket.

    :param seat_numbers: list[str] - list of seat numbers.
    :param flight_id: str - string containing the flight identifier.
    :return: generator - generator that yields 12 character long ticket codes.

    """
    for seat_number in seat_numbers:
        yield (seat_number + flight_id).ljust(12, "0")
