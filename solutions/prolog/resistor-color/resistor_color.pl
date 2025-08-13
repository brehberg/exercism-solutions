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
