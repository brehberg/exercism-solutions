:- use_module(library(clpfd)).

%! beverage(?Chef, ?Drink)
%
% The beverage/2 predicate succeeds with the specific Drink each Chef brought.
beverage(Chef, Drink) :-
    drinks(Index, Drink),
    person(Person, Chef),
    nth1(Person, People, Value),
    nth1(Index, Drinks, Value),
    solve(People, _, Drinks),
    once(label(Drinks)).

%! dish(?Chef, ?Dish)
%
% The dish/2 predicate succeeds with the specific Dish each Chef prepared.
dish(Chef, Dish) :-
    dishes(Index, Dish),
    person(Person, Chef),    
    nth1(Person, People, Value),
    nth1(Index, Dishes, Value),    
    solve(People, Dishes, _),
    once(label(Dishes)).

person(1, aisha).
person(2, emma).
person(3, mei).
person(4, winona).

dishes(1, pad_thai).
dishes(2, frybread).
dishes(3, tagine).
dishes(4, biryani).

drinks(1, tonic).
drinks(2, lassi).
drinks(3, kombucha).
drinks(4, amasi).

solve(Person, Dishes, Drinks) :-

    Person = [Aisha, Emma, Mei, Winona],
    Dishes = [PadThai, Frybread, Tagine, Biryani],
    Drinks = [Tonic, Lassi, Kombucha, Amasi],

    all_distinct(Person),
    all_distinct(Dishes),
    all_distinct(Drinks),

    Person ins 1..4,
    Dishes ins 1..4,
    Drinks ins 1..4,
        
    Aisha #= Tagine,    % Aisha prepares Tagine.
    Emma #= Amasi,      % Emma brings Amasi.
    Frybread #= Tonic,  % The chef who prepares Frybread brings Tonic.
    Mei #= Lassi,       % Mei brings Lassi.
    Winona #\= PadThai, % Winona does not prepare Pad Thai.
    Lassi #\= Biryani,  % The chef who brings the Lassi did not cook the Biryani
    drinks(Kombucha, kombucha).
