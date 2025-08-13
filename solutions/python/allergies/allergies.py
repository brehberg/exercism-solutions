"""Allergies"""


class Allergies:
    """Given a person's allergy score, determine whether or not they're allergic
    to a given item, and their full list of allergies."""

    ALLERGENS = [
        "eggs",
        "peanuts",
        "shellfish",
        "strawberries",
        "tomatoes",
        "chocolate",
        "pollen",
        "cats",
    ]

    def __init__(self, score):
        self.__allergies = [
            self.ALLERGENS[i] for i in range(len(self.ALLERGENS)) if score & (1 << i)
        ]

    def allergic_to(self, allergen):
        """Determine whether the user is allergic or not to some element.
        Return true if allergic to the given type.

        allergen: Allergy type

        """
        return allergen in self.__allergies

    @property
    def lst(self):
        """Return the whole list of allergies."""
        return self.__allergies
