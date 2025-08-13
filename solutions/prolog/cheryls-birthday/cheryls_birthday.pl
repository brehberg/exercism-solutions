% Cheryl gives them a list of 10 possible dates:
all_possible([may-15, may-16, may-19, june-17, june-18, 
    july-14, july-16, august-14, august-15, august-17]).

% Cheryl then tells Albert the Month and
% Bernard the Day of her birthday separately.
cheryls_birthday(Month, Day) :-

    % Albert: I don't know when Cheryl's birthday is,
    % but I know that Bernard doesn't know too. [sic]
    day_groups(ByDaysPart1, []),
    unique(ByDaysPart1, InvalidDaysPart1),              % [18, 19]
    months_with_days(InvalidDaysPart1, InvalidMonths),  % [may, june]

    % Bernard: At first I don't [sic] know when Cheryl's 
    % birthday is, but I know now.
    day_groups(ByDaysPart2, InvalidMonths),
    non_unique(ByDaysPart2, InvalidDaysPart2),          % [14]

    % Albert: Then I also know when Cheryl's birthday is.
    append(InvalidDaysPart1, InvalidDaysPart2, InvalidDays),
    month_groups(ByMonth, InvalidMonths, InvalidDays),
    unique(ByMonth, [Month]),                           % july
    include(key_filter(Month), ByMonth, [_-[Day]]).     % 16


% Result is Birthdays grouped by Day except for given Months
day_groups(Result, ExceptMonths) :-
    all_possible(BirthDays),
    exclude(keylist_filter(ExceptMonths), BirthDays, Dates),
    transpose_pairs(Dates, Transposed),
    group_pairs_by_key(Transposed, Result).

% Result is Birthdays grouped by Month except for given Months and Days
month_groups(Result, ExceptMonths, ExceptDays) :-
    all_possible(BirthDays),
    exclude(keylist_filter(ExceptMonths), BirthDays, Dates1),
    exclude(valuelist_filter(ExceptDays), Dates1, Dates2),
    group_pairs_by_key(Dates2, Result).

% Keys for Grouped Pairs that have only one Value
unique(Grouped, Keys) :-
    include(unique_filter, Grouped, Pairs),
    pairs_keys(Pairs, Keys).
% Keys for Grouped Pairs that have more than one Value
non_unique(Grouped, Keys) :-
    exclude(unique_filter, Grouped, Pairs),
    pairs_keys(Pairs, Keys).
    
% Returns the Months that include the given list of Days
months_with_days(Days, Months) :-
    maplist(months_with_day, Days, Bdays),
    flatten(Bdays, Flat),
    list_to_set(Flat, Months).
% Returns the Months that include the single give Day
months_with_day(Day, Months) :-
    all_possible(Bdays),
    include(value_filter(Day), Bdays, Pairs),
    pairs_keys(Pairs, Months).    

% Various filters used with include and exclude predicates
key_filter(Key,Key-_).
value_filter(Value,_-Value).
keylist_filter(Keys,Key-_) :- member(Key, Keys).
valuelist_filter(Values,_-Value) :- member(Value, Values).
unique_filter(_-Value) :- length(Value, 1).