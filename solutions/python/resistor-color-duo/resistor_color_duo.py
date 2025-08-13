"""Helpful program so that you don't have to remember the values of the bands."""


def value(colors):
    """Calculate a resistance value from two colors"""
    return COLOR_CODES.index(colors[0]) * 10 + COLOR_CODES.index(colors[1])


COLOR_CODES = [
    "black",
    "brown",
    "red",
    "orange",
    "yellow",
    "green",
    "blue",
    "violet",
    "grey",
    "white",
]
