%! divisible_by(+Number, +Divisor)
%
% The divisible_by/2 predicate succeeds if Divisor is a factor 
% of Number determined by using the modulo operation.
divisible_by(N, D) :- 0 is N mod D.

%! leap(+Year)
%
% The leap/1 predicate succeeds if given Year is a leap year.
leap(Year) :- 
    divisible_by(Year, 4),
    \+ divisible_by(Year, 100), !;
    divisible_by(Year, 400).
