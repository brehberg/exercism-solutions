def translate(phrase):
    import re

    return re.sub(
        r"(^|\s+)([^aeiou]*qu|y(?!t)|x(?!r)|[^aeiou]{2,}(?=y)|[^aeiouxy]+)?(\S+)",
        r"\1\3\2ay",
        phrase,
    )
