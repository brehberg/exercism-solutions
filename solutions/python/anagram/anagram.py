"""Find the subset of the candidates that are anagrams."""


def find_anagrams(subject: str, candidates: list[str]) -> list[str]:
    """Returns all candidates that are anagrams of,
    but not equal to, 'subject' word.

    :param subject: str - the word to check
    :param candidates: list[str] - potential matches
    :return: list[str] - anagrams of 'subject'
    """
    base = subject.lower()
    sorted_base = sorted(base)

    def is_anagram(candidate: str) -> bool:
        if len(candidate) != len(base):
            return False
        word = candidate.lower()
        if word == base:
            return False
        return sorted(word) == sorted_base

    return [word for word in candidates if is_anagram(word)]
