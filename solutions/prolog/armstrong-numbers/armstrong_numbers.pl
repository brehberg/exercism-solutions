%! collatz_steps(+N)
%
% The armstrong_number/1 predicate succeeds if given N is a number that is the
% sum of its own digits each raised to the power of the number of digits.
armstrong_number(0) :- !.
armstrong_number(N) :-
    Len is floor(log10(N))+1,
    calc_sum(N, Len, 0, Sum),
    N == Sum.

calc_sum(0, _, Sum, Sum) :- !.
calc_sum(N, Len, S, Sum) :- 
    Digit is N mod 10,
    NextN is N div 10,
    NextS is S+Digit**Len,
    calc_sum(NextN, Len, NextS, Sum).
