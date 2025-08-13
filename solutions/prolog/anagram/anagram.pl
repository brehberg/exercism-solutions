%! is_identical(+Base, +Candidate)
%
% The is_identical/2 predicate succeeds if Base is
% equal to Candidate after converting to lowercase
is_identical(Base, Candidate) :-
    string_lower(Candidate, Word),
    Base == Word.

%! is_anagram(+Sorted, +Candidate)
%
% The is_anagram/2 predicate succeeds if Sorted has
% all the same characters as Candidate in lowercase
is_anagram(Sorted, Candidate) :-
    string_lower(Candidate, Word),
    string_chars(Word, WordChars),
    same_length(Sorted, WordChars),
    msort(WordChars, Sorted).

%! anagram(+Word, +Candidates, -Anagrams)
%
% The anagram/3 predicate succeeds with Anagrams as all 
% Candidates that are anagrams of, but not equal to, Word.
anagram(Word, Candidates, Anagrams) :- 
    string_lower(Word,Base),
    string_chars(Base, BaseChars),
    msort(BaseChars, BaseSorted),
    exclude(is_identical(Base), Candidates, Potentials),
    include(is_anagram(BaseSorted), Potentials, Anagrams).
