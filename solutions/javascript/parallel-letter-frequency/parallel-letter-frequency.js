export const parallelLetterFrequency = (texts) => {
  let frequencies = {};

  const letterFrequency = (text) => {
    return new Promise((resolve) => {
      for (const [letter] of text.toLowerCase().match(/\p{Letter}/gu) || []) {
        frequencies[letter] = (frequencies[letter] || 0) + 1;
      }
      resolve(frequencies);
    });
  };

  // Promises provide concurency but not true parallelism
  Promise.all(texts.map((text) => letterFrequency(text)));
  return frequencies;
};
