export const parallelLetterFrequency = (texts) => {
  const letterFrequency = (frequencies, text) => {
    return new Promise((resolve) => {
      for (const [letter] of text.toLowerCase().match(/\p{Letter}/gu) || []) {
        frequencies[letter] = (frequencies[letter] || 0) + 1;
      }
      resolve(frequencies);
    });
  };

  // Promises provide concurency but not true parallelism
  const result = {};
  Promise.all(texts.map((text) => letterFrequency(result, text)));
  return new Promise((resolve) => resolve(result));
};
