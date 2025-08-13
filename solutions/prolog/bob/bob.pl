%! hey(+Sentence, -Response)
%
% The hey/2 predicate succeeds with Response equal what Bob will reply to someone when
% they say something to him or ask him a question. His responses are pretty limited.
hey(Sentence, Response) :- is_silence(Sentence), silence_reply(Response), !.
hey(Sentence, Response) :- is_yell_question(Sentence), yell_question_reply(Response), !.
hey(Sentence, Response) :- is_yelling(Sentence), yelling_reply(Response), !.
hey(Sentence, Response) :- is_question(Sentence), questionReply(Response), !.
hey(_, Response) :- default_reply(Response).

% The is_silence/1 predicate succeeds if Str contains only whitespace.
is_silence(Str) :- normalize_space(string(Clean), Str), Clean == "".

% The is_yell_question/1 predicate succeeds if Str is yelling and question.
is_yell_question(Str) :- is_question(Str), is_yelling(Str).

% The is_yelling/1 predicate succeeds if Str is ALL CAPITAL LETTERS.
is_yelling(Str) :-
    string_upper(Str, Upper), Str == Upper,
    string_lower(Str, Lower), Str \== Lower.

% The is_question/1 predicate succeeds if Str ends with a question mark.
is_question(Str) :- normalize_space(string(Clean), Str),
    string_chars(Clean, Chars), last(Chars, Last), Last == '?'.

% Bob only ever answers one of five things:    
default_reply("Whatever.").
silence_reply("Fine. Be that way!").
questionReply("Sure.").
yelling_reply("Whoa, chill out!").
yell_question_reply("Calm down, I know what I'm doing!").
