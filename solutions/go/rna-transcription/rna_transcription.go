package strand

// mapping of each DNA nucleotide to its RNA complement
var dnaToRna = map[rune]rune{
	'G': 'C',
	'C': 'G',
	'T': 'A',
	'A': 'U',
}

// ToRNA transcribes a string representing DNA nucleotides to RNA.
func ToRNA(dna string) string {
	rna := make([]rune, len(dna))

	for i, nucleotide := range dna {
		rna[i] = dnaToRna[nucleotide]
	}
	return string(rna)
}
