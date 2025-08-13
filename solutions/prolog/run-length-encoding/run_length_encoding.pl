%! encode(+Plaintext, -Ciphertext)
%
% The encode/2 predicate succeeds with Ciphertext as a string where 
% consecutive elements are represented as a count and data value.
%   For example: "AABBBCCCC" => "2A3B4C"
encode(Plaintext, Ciphertext) :-
    string_chars(Plaintext, Chars),
    clumped(Chars, Pairs),
    foldl(encode_pair, Pairs, [], Encoded),
    string_chars(Ciphertext, Encoded).

%! decode(+Ciphertext, -Plaintext)
%
% The decode/2 predicate succeeds with Plaintext as a string that 
% has been reconstructed from the cipher into its original form.
%   For example: "2A3B4C" => "AABBBCCCC"
decode(Ciphertext, Plaintext) :-
    re_split("[\\d]*[\\D]", Ciphertext, Splits),
    subtract(Splits, [""], Groups),
    foldl(decode_group, Groups, [], Decoded),
    string_chars(Plaintext, Decoded).

% encode_pair/3 converts a Pair into a CharList and appends it 
% to the Accumulator. For example: 'A'-12 => ['1', '2', 'A']
encode_pair(Letter-1, Acc, Encoded) :-
    append(Acc, [Letter], Encoded), !.
encode_pair(Letter-Count, Acc, Encoded) :-
    number_chars(Count, Chars),
    append([Acc, Chars, [Letter]], Encoded).

% decode_group/3 converts a String into a CharList and appends it
% to the Accumulator. For example: "3B" => ['B', 'B', 'B']
decode_group(Str, Acc, Decoded) :-
    string_chars(Str, Chars),
    last(Chars, Letter),
    append(Digits, [Letter], Chars),
    repeated(Letter, Digits, Letters),
    append(Acc, Letters, Decoded), !.

repeated(C, [], L) :- L = [C], !.
repeated(C, Ds, L) :-
    number_chars(N, Ds),
    length(L, N),
    maplist(=(C), L).