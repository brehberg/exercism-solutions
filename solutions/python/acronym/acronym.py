import re


def abbreviate(words):
    """Convert a phrase to its acronym.

    :param words: str - long phrase.
    :return: str - capitalized acronym.
    """
    return re.sub(r"(?<!_)\B[\w']+|[\s\W_]", "", words).upper()
