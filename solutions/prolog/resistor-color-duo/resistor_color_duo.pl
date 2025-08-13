% Calculate a resistance value from two colors
value([Color1, Color2], Value) :-
    color_code(Color1, TensValue),
    color_code(Color2, OnesValue),
    Value is TensValue*10+OnesValue, !.
value([Color1, Color2|_], Value) :- 
    value([Color1, Color2], Value).

% Return the value of a color band
color_code(Color, Code) :-
    colors(Colors),
    nth0(Code, Colors, Color), !.

% Better Be Right Or Your Great Big Values Go Wrong
colors(Colors) :-
    Colors = [
        "black", "brown", "red", "orange", "yellow",
        "green", "blue", "violet", "grey","white"
    ].
