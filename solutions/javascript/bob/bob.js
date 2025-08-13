// @ts-check

// Bob is a lackadaisical teenager. He likes to think that he's very cool.
// And he definitely doesn't get excited about things. That wouldn't be cool.
const reply = {
  default: "Whatever.",
  question: "Sure.",
  yelling: "Whoa, chill out!",
  escalate: "Calm down, I know what I'm doing!",
  silence: "Fine. Be that way!",
};

/**
 * Hey returns a string that only ever answers one of five things.
 * @param {string} message
 * @return {string} response
 */
export const hey = (message) => {
  const isSilence = !/\S/.test(message);
  if (isSilence) return reply.silence;

  const isYelling = /[A-Z]/.test(message) && !/[a-z]/.test(message);
  const isQuestion = /\?\s*$/.test(message);

  return isYelling && isQuestion
    ? reply.escalate
    : isYelling
    ? reply.yelling
    : isQuestion
    ? reply.question
    : reply.default;
};
