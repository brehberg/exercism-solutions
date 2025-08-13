"""Space Age"""

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


class SpaceAge:
    """Given an age in seconds, calculate how old someone would be on each planet."""

    def __init__(self, seconds: int):
        self.__earth_age = seconds / EARTH_YEAR_IN_SECONDS

    def __compute_age(self, planet: str) -> float:
        return round(self.__earth_age / ORBITAL_PERIOD[planet], 2)

    def on_earth(self) -> float:
        """Return age in Earth-years"""
        return self.__compute_age("earth")

    def on_mercury(self) -> float:
        """Return age in Mercury-years"""
        return self.__compute_age("mercury")

    def on_venus(self) -> float:
        """Return age in Venus-years"""
        return self.__compute_age("venus")

    def on_mars(self) -> float:
        """Return age in Mars-years"""
        return self.__compute_age("mars")

    def on_jupiter(self) -> float:
        """Return age in Jupiter-years"""
        return self.__compute_age("jupiter")

    def on_saturn(self) -> float:
        """Return age in Saturn-years"""
        return self.__compute_age("saturn")

    def on_uranus(self) -> float:
        """Return age in Uranus-years"""
        return self.__compute_age("uranus")

    def on_neptune(self) -> float:
        """Return age in Neptune-years"""
        return self.__compute_age("neptune")
