isogram(Phrase) :-
    string_lower(Phrase, Lower),
    string_chars(Lower, Chars),
    include(is_alpha, Chars, Letters),
    is_set(Letters).
