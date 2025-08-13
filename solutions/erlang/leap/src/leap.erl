-module(leap).

-export([leap_year/1]).

leap_year(Year) ->
    Divisible_by = fun(N) -> Year rem N == 0 end,
    Divisible_by(4) and
        not Divisible_by(100) or
        Divisible_by(400).
