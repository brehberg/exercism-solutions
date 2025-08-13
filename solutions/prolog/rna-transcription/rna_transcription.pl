%! dna_2_rna(+DnaChar, +RnaChar).
dna_2_rna('G', 'C').
dna_2_rna('C', 'G').
dna_2_rna('T', 'A').
dna_2_rna('A', 'U').
dna_2_rna(_, _) := fail.

%! rna_transcription(+Dna, +Rna).
rna_transcription(Dna, Rna) :-
  string_chars(Dna, DnaChars),
  maplist(dna_2_rna, DnaChars, RnaChars),  
  string_chars(Rna, RnaChars).
