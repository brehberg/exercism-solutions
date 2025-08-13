%! nucleotide_count(+Strand, -Counts).
nucleotide_count(Str, Counts) :-
    string_chars(Str, Nucleotides),
    foldl(count_chars, Nucleotides, [0, 0, 0, 0], [A, C, G, T]),
    Counts = [('A', A), ('C', C), ('G', G), ('T', T)].

count_chars('A', [A, C, G, T], [A1, C, G, T]) :- A1 is A + 1.
count_chars('C', [A, C, G, T], [A, C1, G, T]) :- C1 is C + 1.
count_chars('G', [A, C, G, T], [A, C, G1, T]) :- G1 is G + 1.
count_chars('T', [A, C, G, T], [A, C, G, T1]) :- T1 is T + 1.
