%! square(+SquareNumber, -Value)
%
% The square/2 predicate succeeds if SquareNumer with Value
% as two to the power of the SquareNumber minus one.
square(SquareNumber, Value) :-
    between(1, 64, SquareNumber),
    Value is 2 ** (SquareNumber - 1).

%! total(-Value)
%
% The total/1 predicate succeeds with Value as the 
% sum of square of each number from 1 to 64.
total(Value) :-
   Value is 2 ** 64 - 1.
