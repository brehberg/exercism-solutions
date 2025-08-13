%! abbreviate(+Sentence, -Acronym)
%
% The abbreviate/2 predicate succeeds with Acronym as
% the first letter of each word in the Sentence. 
% Ex. "This is a string" => "TIAS"
abbreviate(Sentence, Acronym) :-
   split_string(Sentence, "\s-", "\s-_", Words),
   maplist(first, Words, Chars),
   string_chars(Letters, Chars),
   string_upper(Letters, Acronym).

first(Word, Char) :- string_chars(Word, [Char|_]).