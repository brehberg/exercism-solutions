%! hamming_distance(+Strand1, +Strand2, -Distance)
%
% The hamming_distance/3 predicate succeeds with Distance between two DNA strands.
hamming_distance(Str1, Str2, Dist) :-
    string_chars(Str1, Strand1),
    string_chars(Str2, Strand2),
    foldl(hamming, Strand1, Strand2, 0, Dist).    

hamming(Char1, Char2, Dist, Acc) :- 
    (Char1 == Char2 -> Acc is Dist; Acc is Dist + 1).