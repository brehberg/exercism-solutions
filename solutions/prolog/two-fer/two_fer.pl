two_fer(Name, Dialogue) :-
    string_concat("One for ", Name, FirstPart),
    string_concat(FirstPart, ", one for me.", Dialogue).

two_fer(Dialogue) :-
    two_fer("you", Dialogue).
