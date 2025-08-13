/**
 * Translate RNA sequences into proteins.
 *
 * @param {string} strand
 * @returns {string[]}
 */
export const translate = (strand) => {
  const result = [];
  if (!strand) return result;
  for (let chunk of strand.match(/.{1,3}/g)) {
    let aminoAcid = MAPPING[chunk];
    if (!aminoAcid) throw new Error("Invalid codon");
    if (aminoAcid === STOP_CODON) break;
    result.push(aminoAcid);
  }
  return result;
};
const STOP_CODON = "STOP";
/**
 * @type {Record<string,string}
 */
const MAPPING = {
  UGU: "Cysteine",
  UGC: "Cysteine",
  UUA: "Leucine",
  UUG: "Leucine",
  AUG: "Methionine",
  UUU: "Phenylalanine",
  UUC: "Phenylalanine",
  UCU: "Serine",
  UCC: "Serine",
  UCA: "Serine",
  UCG: "Serine",
  UGG: "Tryptophan",
  UAU: "Tyrosine",
  UAC: "Tyrosine",
  UAA: STOP_CODON,
  UAG: STOP_CODON,
  UGA: STOP_CODON,
};
