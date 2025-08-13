%! create(+DimTuple)
%
% The create/1 predicate succeeds if the DimTuple contains valid chessboard 
% dimensions, e.g. (0,0) or (2,4).
create((Rank, File)) :-
	between(0, 7, Rank), between(0, 7, File).

%! attack(+FromTuple, +ToTuple)
%
% The attack/2 predicate succeeds if a queen positioned on ToTuple is 
% vulnerable to an attack by another queen positioned on FromTuple.
attack((Rank, _), (Rank, _)) :- !.
attack((_, File), (_, File)) :- !.

attack((FromRank, FromFile), (ToRank, ToFile)) :-
	create((FromRank, FromFile)),
	create((ToRank, ToFile)),
	abs(FromRank - ToRank) =:= abs(FromFile - ToFile), 
	\+ (FromRank == ToRank, FromFile == ToFile).
