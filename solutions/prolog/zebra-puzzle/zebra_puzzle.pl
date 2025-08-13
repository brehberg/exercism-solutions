:- use_module(library(clpfd)).

% There are five houses.
resident(1, englishman).
resident(2, spaniard).
resident(3, ukranian).
resident(4, norwegian).
resident(5, japanese).
neighbor(X, Y) :- abs(X-Y) #= 1.

solve([Englishman, Spaniard, Ukranian, Norwegian, Japanese], Zebra, Water) :-

    Person = [Englishman, Spaniard, Ukranian, Norwegian, Japanese],
    Colors = [Red, Green, Ivory, Yellow, Blue],
    Animal = [Dog, Snails, Fox, Horse, Zebra],
    Drinks = [Coffee, Tea, Milk, OrangeJuice, Water],
    Smokes = [OldGold, Kools, Chesterfields, LuckyStrike, Parliaments],    

    all_distinct(Person),
    all_distinct(Colors),
    all_distinct(Animal),
    all_distinct(Drinks),
    all_distinct(Smokes),
    
    Person ins 1..5,
    Colors ins 1..5,
    Animal ins 1..5,
    Drinks ins 1..5,
    Smokes ins 1..5,    
    
    Englishman #= Red,  % The Englishman lives in the Red house.
    Spaniard #= Dog,    % The Spaniard owns the Dog.
    Coffee #= Green,    % Coffee is drunk in the Green house.
    Ukranian #= Tea,    % The Ukrainian drinks Tea.
    Green #= Ivory + 1, % The Green house is immediately to the right of the Ivory house.

    OldGold #= Snails,  % The Old Gold smoker owns Snails.
    Kools #= Yellow,    % Kools are smoked in the Yellow house.
    Milk #= 3,          % Milk is drunk in the middle house.
    Norwegian #= 1,     % The Norwegian lives in the first house.
    neighbor(Chesterfields, Fox),  % The man who smokes Chesterfields lives in the house next to the man with the Fox.

    neighbor(Kools, Horse),        % Kools are smoked in the house next to the house where the Horse is kept.
    LuckyStrike #= OrangeJuice,    % The Lucky Strike smoker drinks orange juice.
    Japanese #= Parliaments,       % The Japanese smokes Parliaments.
    neighbor(Norwegian, Blue),     % The Norwegian lives next to the Blue house.
    all_distinct([Water, Zebra]).  % Who drinks water and who owns the zebra?

zebra_owner(Owner) :-
    solve(People, Zebra, _),
    label(People),
    nth1(Person, People, Zebra),
    resident(Person, Owner), !.

water_drinker(Drinker) :-
    solve(People, _, Water),
    label(People),
    nth1(Person, People, Water),
    resident(Person, Drinker), !.
