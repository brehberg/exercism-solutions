"""
A word's Scrabble score is the sum of its letters' values.
"""

from enum import IntEnum


class LetterValue(IntEnum):
    """
    Each letter has a value.
    """

    A = E = I = O = U = L = N = R = S = T = 1
    D = G = 2
    B = C = M = P = 3
    F = H = V = W = Y = 4
    K = 5
    J = X = 8
    Q = Z = 10


def score(word: str) -> int:
    """
    Compute a word's score by summing the values of its letters.
    """
    return sum(LetterValue[c] for c in word.upper())
