class Bob
  # Bob is a lackadaisical teenager. He likes to think that he's very cool.
  # And he definitely doesn't get excited about things. That wouldn't be cool.

  def self.hey(message)
    # Bob returns a string that only ever answers one of five things.
    return SILENCE_REPLY if message.is_silence?
    return YELLING_QUESTION_REPLY if message.is_yelling_question?
    return YELLING_REPLY if message.is_yelling?
    return QUESTION_REPLY if message.is_question?
    return DEFAULT_REPLY
  end

  DEFAULT_REPLY = "Whatever."
  QUESTION_REPLY = "Sure."
  YELLING_REPLY = "Whoa, chill out!"
  YELLING_QUESTION_REPLY = "Calm down, I know what I'm doing!"
  SILENCE_REPLY = "Fine. Be that way!"
end

class String
  def is_silence?
    self.strip.empty?
  end

  def is_yelling?
    self.match(/\p{Upper}/) and not self.match(/\p{Lower}/)
  end

  def is_question?
    self.strip.end_with? "?"
  end

  def is_yelling_question?
    self.is_yelling? and self.is_question?
  end
end
