"""Matching Brackets"""

MATCHES = {"[": "]", "{": "}", "(": ")"}


def is_paired(input_string):
    """Checks that all the brackets and braces in the string are matched correctly,
    and nested correctly"""
    closer_needed = []

    for c in input_string:
        if c in MATCHES.values():
            # closing bracket was found, is it the next expected value on stack?
            if not closer_needed or closer_needed.pop() != c:
                return False
        elif c in MATCHES:
            # opening bracket was found, add matching closing value to the stack
            closer_needed.append(MATCHES[c])

    return not closer_needed
