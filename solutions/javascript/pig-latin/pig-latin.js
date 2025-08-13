const translateWord = (word) => {
  // If a word begins with a vowel sound, add
  // an "ay" sound to the end of the word
  let match = word.match(/^(?:[aeiou]|xr|yt)/);
  if (match) return `${word}ay`;

  // If a word begins with a consonant sound, move
  // it to the end of the word and then add "ay"
  match = word.match(/^(.*qu|[^aeiou]+)([aeiouy].*)/);
  if (match) return `${match[2]}${match[1]}ay`;

  // are there any other cases?
  return word;
};

export const translate = (phrase) => {
  return phrase.split(/\s+/).map(translateWord).join(" ");
};
