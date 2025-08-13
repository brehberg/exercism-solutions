// @ts-check

// Bob is a lackadaisical teenager. He likes to think that he's very cool.
// And he definitely doesn't get excited about things. That wouldn't be cool.

// Bob returns a string that only ever answers one of five things.
const reply = {
  default: "Whatever.",
  question: "Sure.",
  yelling: "Whoa, chill out!",
  yellingQuestion: "Calm down, I know what I'm doing!",
  silence: "Fine. Be that way!",
};

export const hey = (/** @type {string} */ message) => {
  const isSilence = !/\S/.test(message);
  if (isSilence) return reply.silence;

  const isYelling = /[A-Z]/.test(message) && !/[a-z]/.test(message);
  const isQuestion = /\?\s*$/.test(message);

  return isYelling && isQuestion
    ? reply.yellingQuestion
    : isYelling
    ? reply.yelling
    : isQuestion
    ? reply.question
    : reply.default;
};
