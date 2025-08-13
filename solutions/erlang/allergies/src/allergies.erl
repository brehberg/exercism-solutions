-module(allergies).

-export([is_allergic_to/2, allergies/1, test_version/0]).

is_allergic_to(Allergy, Score) ->
    lists:member(Allergy, allergies(Score)).

allergies(Score) ->
    allergies(Score, []).

allergies(Score, Allergies) when (Score >= 256) ->
    allergies(Score - 256, Allergies);
allergies(Score, Allergies) when (Score >= 128) ->
    allergies(Score - 128, ['cats' | Allergies]);
allergies(Score, Allergies) when (Score >= 64) ->
    allergies(Score - 64, ['pollen' | Allergies]);
allergies(Score, Allergies) when (Score >= 32) ->
    allergies(Score - 32, ['chocolate' | Allergies]);
allergies(Score, Allergies) when (Score >= 16) ->
    allergies(Score - 16, ['tomatoes' | Allergies]);
allergies(Score, Allergies) when (Score >= 8) ->
    allergies(Score - 8, ['strawberries' | Allergies]);
allergies(Score, Allergies) when (Score >= 4) ->
    allergies(Score - 4, ['shellfish' | Allergies]);
allergies(Score, Allergies) when (Score >= 2) ->
    allergies(Score - 2, ['peanuts' | Allergies]);
allergies(Score, Allergies) when (Score >= 1) ->
    allergies(Score - 1, ['eggs' | Allergies]);
allergies(_Score, Allergies) ->
    Allergies.

test_version() -> 1.
