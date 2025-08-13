-module(meetup).

-export([meetup/4]).

meetup(Year, Month, DayOfWeek, last) ->
    LastDay = calendar:last_day_of_the_month(Year, Month),
    adjust_day_in_week(DayOfWeek, {Year, Month, LastDay}, -1);
meetup(Year, Month, DayOfWeek, Week) ->
    adjust_day_in_week(DayOfWeek, {Year, Month, first_day(Week)}, 1).

adjust_day_in_week(DayOfWeek, Guess, Dir) ->
    Offset = (day_num(DayOfWeek) - calendar:day_of_the_week(Guess) + 7 * Dir) rem 7,
    calendar:gregorian_days_to_date(calendar:date_to_gregorian_days(Guess) + Offset).

day_num(monday) -> 1;
day_num(tuesday) -> 2;
day_num(wednesday) -> 3;
day_num(thursday) -> 4;
day_num(friday) -> 5;
day_num(saturday) -> 6;
day_num(sunday) -> 7.

first_day(first) -> 1;
first_day(second) -> 8;
first_day(third) -> 15;
first_day(fourth) -> 22;
first_day(teenth) -> 13.
