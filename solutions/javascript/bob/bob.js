// @ts-check

// Bob is a lackadaisical teenager. He likes to think that he's very cool.
// And he definitely doesn't get excited about things. That wouldn't be cool.

// Bob returns a string that only ever answers one of five things.
const defaultReply = "Whatever.";
const questionReply = "Sure.";
const yellingReply = "Whoa, chill out!";
const yellingQuestionReply = "Calm down, I know what I'm doing!";
const silenceReply = "Fine. Be that way!";

export const hey = (/** @type {string} */ message) => {
  const isSilence = message?.match(/\S/) === null;
  if (isSilence) return silenceReply;

  const isYelling = message.match(/[A-Z]/) && !message.match(/[a-z]/);
  const isQuestion = message.match(/\?\s*$/) !== null;

  return isYelling && isQuestion
    ? yellingQuestionReply
    : isYelling
    ? yellingReply
    : isQuestion
    ? questionReply
    : defaultReply;
};
