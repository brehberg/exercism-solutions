%! collatz_steps(+N, -Steps)
%
% The collatz_steps/2 predicate succeeds with Steps required to reach 1.
collatz_steps(N, Steps) :-
    do_collatz_step(N, 0, Steps).

% If n is one, the steps are done
do_collatz_step(1, S, Steps) :- Steps is S, !.

% If n is even, divide n by 2 
do_collatz_step(N, S, Steps) :-
    N > 1, 0 is N mod 2, NextN is N div 2,
    do_collatz_step(NextN, S + 1, Steps), !.

% If n is odd, multiply n by 3 and add 1
do_collatz_step(N, S, Steps) :-
    N > 1, NextN is N * 3 + 1,
    do_collatz_step(NextN, S + 1, Steps).