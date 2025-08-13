"""Helpful program so that you don't have to remember the values of the bands."""


def color_code(color):
    """Return the value of a color band"""
    return colors().index(color)


def colors():
    """Better Be Right Or Your Great Big Values Go Wrong"""
    return [
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
