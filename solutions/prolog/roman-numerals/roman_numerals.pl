
replace(1000, 'M').
replace(900, 'CM').
replace(500, 'D').
replace(400, 'CD').
replace(100, 'C').
replace(90, 'XC').
replace(50, 'L').
replace(40, 'XL').
replace(10, 'X').
replace(9, 'IX').
replace(5, 'V').
replace(4, 'IV').
replace(1, 'I').

convert(0, '') :- !.
convert(N, Numeral) :-
    between(1, 3999, N),
    replace(Value, Pattern),
    N >= Value,
    NextN is N - Value,
    convert(NextN, NextNumeral),
    string_concat(Pattern, NextNumeral, Numeral).