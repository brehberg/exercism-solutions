% A complex number is a number in the form a + b * i
% where a and b are real and i satisfies i^2 = -1.
real((A, _B), R) :- R = A.
imaginary((_A, B), I) :- I = B.

% conjugate of a + b * i is a - b * i
conjugate(C, Conjugate) :-
    real(C, A), imaginary(C, B),
    real(Conjugate, A), imaginary(Conjugate, 0-B).

% absolute value of complex a + b * i is real sqrt(a^2 + b^2)
abs(C1, Abs) :-
    real(C1, A), imaginary(C1, B),
    Abs is sqrt(A*A+B*B).

% Addition (a + i * b) + (c + i * d) = (a + c) + (b + d) * i
add(C1, C2, CAdd) :-
    real(C1, A), imaginary(C1, B),
    real(C2, C), imaginary(C2, D),
    real(CAdd, A+C), imaginary(CAdd, B+D).

% Subtraction (a + i * b) - (c + i * d) = (a - c) + (b - d) * i.
sub(C1, C2, CSub) :-    
    real(C1, A), imaginary(C1, B),
    real(C2, C), imaginary(C2, D),
    real(CSub, A-C), imaginary(CSub, B-D).

% Multiplication (a + i * b) * (c + i * d) 
%   = (a * c - b * d) + (b * c + a * d) * i
mul(C1, C2, CMul) :-
    real(C1, A), imaginary(C1, B),
    real(C2, C), imaginary(C2, D),
    real(CMul, A*C-B*D), imaginary(CMul, B*C+A*D).

% Division (a + i * b) / (c + i * d) 
%   = (a * c + b * d)/(c^2 + d^2) + (b * c - a * d)/(c^2 + d^2) * i
div(C1, C2, CDiv) :-     
    real(C1, A), imaginary(C1, B),
    real(C2, C), imaginary(C2, D),
    real(CDiv, (A*C+B*D)/(C*C+D*D)),
    imaginary(CDiv, (B*C-A*D)/(C*C+D*D)).
