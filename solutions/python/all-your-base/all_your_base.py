def rebase(input_base: int, digits: list[int], output_base: int) -> list[int]:
    """Convert a number, represented as a sequence of digits in one base, to any other base."""

    if input_base < 2:
        raise ValueError("input base must be >= 2")
    if output_base < 2:
        raise ValueError("output base must be >= 2")

    # convert sequence of digits in input base to whole integer value
    positional_offset = len(digits) - 1

    def positional_value(digit: int, index: int):
        if digit < 0 or digit >= input_base:
            raise ValueError("all digits must satisfy 0 <= d < input base")
        return digit * input_base ** (positional_offset - index)

    value = sum(positional_value(d, i) for i, d in enumerate(digits))

    # convert whole integer value to sequence of digits in output base
    result = []
    while value >= output_base:
        result.append(value % output_base)
        value //= output_base

    result.append(value)
    result.reverse()
    return result
