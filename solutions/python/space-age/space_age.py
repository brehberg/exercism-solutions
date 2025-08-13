"""Space Age"""

import functools


class SpaceAge:
    """Given an age in seconds, calculate how old someone would be on each planet."""

    EARTH_YEAR_IN_SECONDS: int = 31557600
    ORBITAL_PERIOD: dict[str, float] = {
        "mercury": 0.2408467,
        "venus": 0.61519726,
        "earth": 1.0,
        "mars": 1.8808158,
        "jupiter": 11.862615,
        "saturn": 29.447498,
        "uranus": 84.016846,
        "neptune": 164.79132,
    }

    def __init__(self, seconds: int):
        self.__earth_age = seconds / self.EARTH_YEAR_IN_SECONDS

    def __compute_age(self, planet: str) -> float:
        return round(self.__earth_age / self.ORBITAL_PERIOD[planet], 2)

    def __getattr__(self, attr_name: str):
        if attr_name.startswith("on_"):
            planet = attr_name[3:]
            if planet in self.ORBITAL_PERIOD:
                return functools.partial(self.__compute_age, planet)
        raise AttributeError
