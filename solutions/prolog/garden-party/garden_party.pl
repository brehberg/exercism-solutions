:- use_module(library(clpfd)).

%! beverage(?Chef, ?Drink)
%
% The beverage/2 predicate succeeds with the specific Drink each Chef brought.
beverage(Chef, Drink) :-
    solve(People, _, [1,2,3,4]),
    chefs(Person, Chef),
    nth1(Person, People, Value),
    drinks(Value, Drink).

%! dish(?Chef, ?Dish)
%
% The dish/2 predicate succeeds with the specific Dish each Chef prepared.
dish(Chef, Dish) :-
    solve(People, [1,2,3,4], _),
    chefs(Person, Chef),
    nth1(Person, People, Value),
    dishes(Value, Dish).

% Four chefs: Aisha, Emma, Mei, and Winona
chefs(1, aisha).
chefs(2, emma).
chefs(3, mei).
chefs(4, winona).
% The dishes are Pad Thai, Frybread, Tagine, and Biryani.
dishes([pad_thai, frybread, tagine, biryani]).
dishes(I, Dish) :- dishes(D), nth1(I, D, Dish).
% The beverages are Tonic, Lassi, Kombucha, and Amasi.
drinks([tonic, lassi, kombucha, amasi]).
drinks(I, Drink) :- drinks(D), nth1(I, D, Drink).

solve(Person, Dishes, Drinks) :-
    Person = [Aisha, Emma, Mei, Winona],
    Dishes = [PadThai, Frybread, Tagine, Biryani],
    Drinks = [Tonic, Lassi, _Kombucha, Amasi],

    Groups = [Person, Dishes, Drinks],
    maplist(all_distinct, Groups),
    append(Groups, Values),
    Values ins 1..4,

    Aisha #= Tagine,    % Aisha prepares Tagine.
    Emma #= Amasi,      % Emma brings Amasi.
    Frybread #= Tonic,  % The chef who prepares Frybread brings Tonic.
    Mei #= Lassi,       % Mei brings Lassi.
    Winona #\= PadThai, % Winona does not prepare Pad Thai.
    Lassi #\= Biryani.  % The chef who brings the Lassi did not cook the Biryani
