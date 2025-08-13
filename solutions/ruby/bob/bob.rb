class Bob
  # Bob is a lackadaisical teenager. He likes to think that he's very cool.
  # And he definitely doesn't get excited about things. That wouldn't be cool.

  def self.hey(message)
    # Bob returns a string that only ever answers one of five things.
    return REPLY[:silence] if message.is_silence?
    return REPLY[:yelling_question] if message.is_yelling_question?
    return REPLY[:yelling] if message.is_yelling?
    return REPLY[:question] if message.is_question?
    return REPLY[:default]
  end

  REPLY = {
    default: "Whatever.",
    question: "Sure.",
    yelling: "Whoa, chill out!",
    yelling_question: "Calm down, I know what I'm doing!",
    silence: "Fine. Be that way!",
  }
  private_constant :REPLY
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
