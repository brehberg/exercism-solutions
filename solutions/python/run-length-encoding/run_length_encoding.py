"""Run-Length Encoding"""

import re


def decode(ciphertext: str) -> str:
    "Decode returns a string that has been reconstructed from the input into its original form."
    return re.sub(r"(\d+)(.)", lambda m: m.group(2) * int(m.group(1)), ciphertext)


def encode(plaintext: str) -> str:
    "Encode returns a string where consecutive elements are represented as a count and value."
    return re.sub(r"(.)\1+", lambda m: f"{len(m.group(0))}{m.group(1)}", plaintext)
