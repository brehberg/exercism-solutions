triangle(A, B, C, _) :- 
    (A + B < C; B + C < A; C + A < B), !, fail.

triangle(A, B, C, _) :- 
    A = 0, B = 0, C = 0, !, fail.

triangle(A, A, A, "equilateral") :- !.

triangle(A, B, C, "scalene") :- 
    A \= B, B \= C, C \= A, !.

triangle(A, A, _, "isosceles") :- !.
triangle(A, _, A, "isosceles") :- !.
triangle(_, A, A, "isosceles") :- !.
