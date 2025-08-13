%! binary(+Str, -Dec)
%
% The binary/2 predicate succeeds with Decimal equivalent of 
% a binary number, represented as a String (e.g. '101010')
binary(Str, Dec) :-    
    string_codes(Str, Codes),    
    % reverse(Codes, Reversed),
    length(Codes, Len),
    % numlist(1, Len, Positions),
    calc_dec(Codes, Len, 0, Dec).
    % maplist(calc_dec, Reversed, Positions, Values),
    % sum_list(Values, Dec).

calc_dec([], 0, Sum, Sum) :- !.
calc_dec([N|R], Pos, S, Sum) :-
    between(48, 49, N),
    NextPos is Pos - 1,
    NextS is S+(N-48)*2**(NextPos),
    calc_dec(R, NextPos, NextS, Sum).


    % calc_dec(N, Pos, Val) :-
    %     between(48, 49, N),
    %     Val is (N-48)*2**(Pos-1).
