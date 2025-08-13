%! binary(+Str, -Dec)
%
% The binary/2 predicate succeeds with Decimal equivalent of 
% a binary number, represented as a String (e.g. '101010')
binary(Str, Dec) :-    
    string_codes(Str, Chars),    
    reverse(Chars, Reversed),
    length(Reversed, Len),
    numlist(1, Len, Positions),
    maplist(calc_dec, Reversed, Positions, Values),
    sum_list(Values, Dec).

calc_dec(N, Pos, Val) :-
    between(48, 49, N),
    Val is (N-48)*2**(Pos-1).
