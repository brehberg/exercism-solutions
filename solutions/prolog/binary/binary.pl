%! binary(+Str, -Dec)
%
% The binary/2 predicate succeeds with Decimal equivalent of 
% a binary number, represented as a String (e.g. '101010')
binary(Str, Dec) :-    
    string_codes(Str, Codes),
    maplist(between(48, 49), Codes),
    length(Codes, Pos),
    calc_dec(Codes, Pos, 0, Dec).

calc_dec([], 0, Sum, Sum) :- !.
calc_dec([N|R], P, S, Sum) :-    
    Value is N - 48,
    NextP is P - 1,
    NextS is S + Value*2**(NextP),
    calc_dec(R, NextP, NextS, Sum).
