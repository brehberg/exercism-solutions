%! square_of_sum(+N, -Result)
%
% The square_of_sum/2 predicate succeeds with Result
% as square of sum from 1 to a given end number N.
square_of_sum(N, Result) :-
    Result is ((N+N*N)/2)**2.

%! sum_of_squares(+N, -Result)
%
% The sum_of_squares/2 predicate succeeds with Result
% as sum of squares from 1 to a given end number N.
sum_of_squares(N, Result) :-
    Result is (N+3*N**2+2*N**3)/6.

%! difference(+N, -Result)
%
% The difference/2 predicate succeeds with Result as the
% difference between sum of squares and square of sum.
difference(N, Result) :-
    square_of_sum(N, Result1),
    sum_of_squares(N, Result2),
    Result is Result1 - Result2.