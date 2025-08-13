"""solve the Zebra Puzzle to find the answers"""

from itertools import permutations

# The order of these values will impact the runtime to find the solution.
# The residents and house colors have already been matched correctly.
# Moving zebra to be last pet and water to be first drink will also make
# those values match corrertly and further decrease the runtime.
residents = ["Norwegian", "Ukrainian", "Englishman", "Spaniard", "Japanese"]
yellow, blue, red, ivory, green = range(5)
zebra, fox, horse, snails, dog = range(5)
tea, milk, orange_juice, coffee, water = range(5)
football, chess, painting, reading, dancing = range(5)


def drinks_water():
    """Which of the residents drinks water?"""
    for house in solution():
        if house["drink"] == water:
            return house["nation"]
    return None


def owns_zebra():
    """Who owns the zebra?"""
    for house in solution():
        if house["pet"] == zebra:
            return house["nation"]
    return None


def solution():
    "Generate possible combinations and evaluate each to find match"
    for nations in permutations(residents):
        for colors in permutations(range(5)):
            for pets in permutations(range(5)):
                for drinks in permutations(range(5)):
                    for hobbies in permutations(range(5)):
                        result = check_solution(nations, colors, pets, drinks, hobbies)
                        if result:
                            return result
    return EMPTY_SOLUTION


def check_solution(nations, colors, pets, drinks, hobbies):
    "Check if possible solution is valid for all known statements"
    houses = [
        {
            "nation": nations[i],
            "color": colors[i],
            "pet": pets[i],
            "drink": drinks[i],
            "hobby": hobbies[i],
        }
        for i in range(5)
    ]
    return houses if all(func(houses) for func in CHECK_FUNCS) else None


def known1(houses):
    """There are five houses."""
    return len(houses) == 5


def known2(houses):
    """The Englishman lives in the red house."""
    return any(
        house["nation"] == "Englishman" and house["color"] == red for house in houses
    )


def known3(houses):
    """The Spaniard owns the dog."""
    return any(
        house["nation"] == "Spaniard" and house["pet"] == dog for house in houses
    )


def known4(houses):
    """The person in the green house drinks coffee."""
    return any(house["drink"] == coffee and house["color"] == green for house in houses)


def known5(houses):
    """The Ukrainian drinks tea."""
    return any(
        house["nation"] == "Ukrainian" and house["drink"] == tea for house in houses
    )


def known6(houses):
    """The green house is immediately to the right of the ivory house."""
    return any(
        houses[i + 1]["color"] == green and houses[i]["color"] == ivory
        for i in range(4)
    )


def known7(houses):
    """The snail owner likes to go dancing."""
    return any(house["hobby"] == dancing and house["pet"] == snails for house in houses)


def known8(houses):
    """The person in the yellow house is a painter."""
    return any(
        house["hobby"] == painting and house["color"] == yellow for house in houses
    )


def known9(houses):
    """The person in the middle house drinks milk."""
    return houses[2]["drink"] == milk


def known10(houses):
    """The Norwegian lives in the first house."""
    return houses[0]["nation"] == "Norwegian"


def known11(houses):
    """The person who enjoys reading lives in the house next to the person with the fox."""
    for i in range(5):
        if houses[i]["hobby"] == reading:
            if i > 0 and houses[i - 1]["pet"] == fox:
                return True
            if i < 4 and houses[i + 1]["pet"] == fox:
                return True
            return False
    return False


def known12(houses):
    """The painter's house is next to the house with the horse."""
    for i in range(5):
        if houses[i]["hobby"] == painting:
            if i > 0 and houses[i - 1]["pet"] == horse:
                return True
            if i < 4 and houses[i + 1]["pet"] == horse:
                return True
            return False
    return False


def known13(houses):
    """The person who plays football drinks orange juice."""
    return any(
        house["hobby"] == football and house["drink"] == orange_juice
        for house in houses
    )


def known14(houses):
    """The Japanese person plays chess."""
    return any(
        house["nation"] == "Japanese" and house["hobby"] == chess for house in houses
    )


def known15(houses):
    """The Norwegian lives next to the blue house."""
    for i in range(5):
        if houses[i]["nation"] == "Norwegian":
            if i > 0 and houses[i - 1]["color"] == blue:
                return True
            if i < 4 and houses[i + 1]["color"] == blue:
                return True
            return False
    return False


CHECK_FUNCS = [
    known1,
    known2,
    known3,
    known4,
    known5,
    known6,
    known7,
    known8,
    known9,
    known10,
    known11,
    known12,
    known13,
    known14,
    known15,
]


EMPTY_SOLUTION = [
    {
        "nation": "Unknown",
        "color": -1,
        "pet": -1,
        "drink": -1,
        "hobby": -1,
    }
]
