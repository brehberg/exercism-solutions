"""Bob returns a string that only ever answers one of five things."""

DEFAULT_REPLY = "Whatever."
QUESTION_REPLY = "Sure."
YELLING_REPLY = "Whoa, chill out!"
YELLING_QUESTION_REPLY = "Calm down, I know what I'm doing!"
SILENCE_REPLY = "Fine. Be that way!"


def response(hey_bob: str) -> str:
    """Bob is a lackadaisical teenager. He likes to think that he's very cool.
    And he definitely doesn't get excited about things. That wouldn't be cool."""

    is_silence = not hey_bob.strip()
    if is_silence:
        return SILENCE_REPLY

    is_yelling = hey_bob.upper() == hey_bob and hey_bob.lower() != hey_bob
    is_question = hey_bob.rstrip().endswith("?")

    if is_yelling and is_question:
        return YELLING_QUESTION_REPLY
    if is_yelling:
        return YELLING_REPLY
    if is_question:
        return QUESTION_REPLY
    return DEFAULT_REPLY
