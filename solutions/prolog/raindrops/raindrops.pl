make_sound(N, "PlingPlangPlong") :- 
    N mod 3 =:= 0, N mod 5 =:= 0, N mod 7 =:= 0, !.
make_sound(N, "PlangPlong") :- 
    N mod 5 =:= 0, N mod 7 =:= 0, !.
make_sound(N, "PlingPlong") :- 
    N mod 3 =:= 0, N mod 7 =:= 0, !.
make_sound(N, "Plong") :- 
    N mod 7 =:= 0, !.
make_sound(N, "PlingPlang") :- 
    N mod 3 =:= 0, N mod 5 =:= 0, !.
make_sound(N, "Plang") :- 
    N mod 5 =:= 0, !.
make_sound(N, "Pling") :- 
    N mod 3 =:= 0, !.
make_sound(N, Sound) :- 
    number_string(N, Sound).

convert(N, Sounds) :- make_sound(N, Sounds).
