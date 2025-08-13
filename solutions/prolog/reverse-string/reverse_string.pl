string_reverse(Str, Reversed) :-
    string_chars(Str, Chars),
    do_reverse(Chars, [], ReversedChars),
    string_chars(Reversed, ReversedChars).

do_reverse([Head|Tail],Acc,Reverse) :- 
    do_reverse(Tail,[Head|Acc],Reverse).
do_reverse([],Acc,Acc).
