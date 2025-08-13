translate(English, PigLatin) :-
    split_string(English, " ", " ", Words),
    maplist(translate_word, Words, PigWords),
    atomics_to_string(PigWords, " ", PigLatin).

translate_word(English, PigLatin) :-
    string_chars(English, Chars),
    translate_chars(Chars, PigLatinChars),
    string_chars(PigLatin, PigLatinChars).

translate_chars(Cs, PigLatin) :-
    starts_with_vowel(Cs), !,
    append(Cs, [a,y], PigLatin).

translate_chars(Cs, PigLatin) :-
    consonants_cluster(Cs, Cluster),
    append(Cluster, Rest, Cs),
    append([Rest, Cluster, [a,y]], PigLatin).

consonants_cluster([q,u|_], [q,u]) :-
    !.
consonants_cluster(Cs, [C|Prefix]) :-
    \+ starts_with_vowel(Cs),
    Cs = [C|Cs1],
    consonants_prefix(Cs1, Prefix).

consonants_prefix([q,u|_], [q,u]) :-
    !.
consonants_prefix(Cs, Prefix) :-
    prefix(Prefix, Cs),
    maplist(consonant, Prefix).

vowel(C) :- member(C, [a,e,i,o,u]).

consonant(C) :- \+ vowel(C).

starts_with_vowel([C|_]) :- vowel(C).
starts_with_vowel([x,r|_]).
starts_with_vowel([y,t|_]).