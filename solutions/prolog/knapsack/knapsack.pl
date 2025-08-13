:- use_module(library(clpfd)).

maximum_value([], Capacity, 0) :- !.

maximum_value(Items, Capacity, Value) :-
    length(Items, N),
    length(Quantity, N),
    Quantity ins 0..1,
    maplist(arg(1), Items, Weights),
    maplist(arg(2), Items, Values),
    sum(Quantity, #>, 0),
    scalar_product(Weights, Quantity, #=<, Capacity),
    scalar_product(Values, Quantity, #=, Value),
    labeling([max(Value)], Quantity),
    !.
