"""Bob returns a string that only ever answers one of five things."""

DEFAULT_REPLY = "Whatever."
QUESTION_REPLY = "Sure."
YELLING_REPLY = "Whoa, chill out!"
YELLING_QUESTION_REPLY = "Calm down, I know what I'm doing!"
SILENCE_REPLY = "Fine. Be that way!"


def response(message: str) -> str:
    """Bob is a lackadaisical teenager. He likes to think that he's very cool.
    And he definitely doesn't get excited about things. That wouldn't be cool."""

    is_silence = not message.strip()
    if is_silence:
        return SILENCE_REPLY

    is_yelling = message.isupper()
    is_question = message.rstrip().endswith("?")

    return (
        (YELLING_QUESTION_REPLY if is_question else YELLING_REPLY)
        if is_yelling
        else QUESTION_REPLY if is_question else DEFAULT_REPLY
    )
