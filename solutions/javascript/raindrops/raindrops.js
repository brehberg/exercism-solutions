export const convert = (num) =>
  (num % 3 ? "" : "Pling") +
    (num % 5 ? "" : "Plang") +
    (num % 7 ? "" : "Plong") || String(num);
