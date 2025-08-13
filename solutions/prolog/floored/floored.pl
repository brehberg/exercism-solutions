:- use_module(library(clpfd)).

%! floor(?Person, ?Floor)
%
% The floor/2 predicate succeeds with the specific Floor each Person lives on.
floor(Person, Floor) :-
    position(Person, Index),
    nth1(Index, People, Floor),
    solve(People),
    label(People).

position(amara, 1).
position(bjorn, 2).
position(cora, 3).
position(dale, 4).
position(emiko, 5).

solve([Amara, Björn, Cora, Dale, Emiko]) :-

    Person = [Amara, Björn, Cora, Dale, Emiko],
    Person ins 1..5,       % Five floors: 1 (bottom), 2, 3, 4 and 5 (top)
    all_distinct(Person),  % Each tenant lives on a different floor
    Amara #\= 5,           % Amara does not live on the top floor
    Björn #\= 1,           % Björn does not live on the bottom floor
    Cora in 2..4,          % Cora does not live on the top or the bottom floor
    Dale #> Björn,         % Dale lives on a higher floor than Björn
    abs(Emiko-Cora) #\= 1, % Emiko does not live on a floor adjacent to Cora
    abs(Cora-Björn) #\= 1. % Cora does not live on a floor adjacent to Björn
