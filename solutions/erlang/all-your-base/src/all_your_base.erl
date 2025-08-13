-module(all_your_base).

-export([rebase/3]).

% Given a number in input base, represented as a sequence of digits, converts it to output base,
% or returns an error tuple if either of the bases are less than 2
rebase(_, InputBase, _) when (InputBase < 2) ->
    {error, "input base must be >= 2"};
rebase(_, _, OutputBase) when (OutputBase < 2) ->
    {error, "output base must be >= 2"};
rebase(Digits, InputBase, OutputBase) ->
    case valid_digits(Digits, InputBase, true) of
        false ->
            {error, "all digits must satisfy 0 <= d < input base"};
        true ->
            integer_to_output(
                input_to_integer(
                    Digits,
                    InputBase,
                    string:length(Digits) - 1,
                    0
                ),
                OutputBase,
                []
            )
    end.

% convert sequence of digits in input base to whole integer value
input_to_integer([], _, _, Result) ->
    Result;
input_to_integer([Digit | Rest], Base, Position, Result) ->
    input_to_integer(Rest, Base, Position - 1, Result + trunc(Digit * math:pow(Base, Position))).

% convert whole integer value to sequence of digits in output base
integer_to_output(Value, Base, Result) when Value < Base ->
    {ok, [Value | Result]};
integer_to_output(Value, Base, Result) ->
    integer_to_output(Value div Base, Base, [Value rem Base | Result]).

% check all digits are non-negative and less than input base
valid_digits([], _, Result) ->
    Result;
valid_digits([Digit | Rest], Base, Valid) when Digit >= 0 andalso Digit < Base ->
    valid_digits(Rest, Base, Valid);
valid_digits(_, _, _) ->
    false.
