
replace(N, (Value, Chars)) :-
    (N >= 4000 -> fail;
     N >= 1000 -> (Value, Chars) = (1000, 'M');
     N >=  900 -> (Value, Chars) = (900, 'CM');
     N >=  500 -> (Value, Chars) = (500, 'D');
     N >=  400 -> (Value, Chars) = (400, 'CD');
     N >=  100 -> (Value, Chars) = (100, 'C');
     N >=   90 -> (Value, Chars) = (90, 'XC');
     N >=   50 -> (Value, Chars) = (50, 'L');
     N >=   40 -> (Value, Chars) = (40, 'XL');     
     N >=   10 -> (Value, Chars) = (10, 'X');
     N >=    9 -> (Value, Chars) = (9, 'IX');
     N >=    5 -> (Value, Chars) = (5, 'V');
     N >=    4 -> (Value, Chars) = (4, 'IV');
     N >=    1 -> (Value, Chars) = (1, 'I');
     N <     0 -> fail).

convert(0, "") :- !.
convert(N, Numeral) :-
    replace(N, (Value, Pattern)),
    NextN is N - Value,
    convert(NextN, NextNumeral),
    string_concat(Pattern, NextNumeral, Numeral).

