% Transforms an old Scrabble score system to a new one.
%   [1-["A", "E"], 2-["D", "G"]] => [a-1, d-2, e-1, g-2]
transform(ScoreLetters, LetterScores) :-
    maplist(convert_pair, ScoreLetters, PairScores),
    flatten(PairScores, FlatScores),
    keysort(FlatScores, LetterScores).

% Convert Pair of Value and Letters List into Letter-Value Pairs
%   For example: 1-["A", "E"] => [a-1, e-1]
convert_pair(Value-Letters, List) :-
    length(Letters, Len),
    length(Values, Len),
    maplist(=(Value), Values),
    foldl(convert_letter, Letters, Values, [], List).

% Accumulate single Value and Letter into List of Letter-Value Pairs
convert_letter(Letter, Value, Acc, List) :- 
    string_lower(Letter, Lower),
    string_to_atom(Lower, Key),    
    append(Acc, [(Key-Value)], List).