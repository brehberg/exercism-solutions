-module(protein_translation).

-export([proteins/1]).

-define(PROTEINS, [
    {"AUG", methionine},
    {"UGG", tryptophan},
    {"UUU", phenylalanine},
    {"UUC", phenylalanine},
    {"UUA", leucine},
    {"UUG", leucine},
    {"UCU", serine},
    {"UCC", serine},
    {"UCA", serine},
    {"UCG", serine},
    {"UAU", tyrosine},
    {"UAC", tyrosine},
    {"UGU", cysteine},
    {"UGC", cysteine},
    {"UAA", stopCodon},
    {"UAG", stopCodon},
    {"UGA", stopCodon}
]).

proteins(_Strand) ->
    from_codons(_Strand, []).

from_codons([X, Y, Z | Rest], Acc) ->
    case proplists:get_value([X, Y, Z], ?PROTEINS) of
        stopCodon -> from_codons([], Acc);
        undefined -> {error, badarg};
        Protein -> from_codons(Rest, [Protein | Acc])
    end;
from_codons([], Acc) ->
    {ok, lists:reverse(Acc)};
from_codons(_, _) ->
    {error, badarg}.
