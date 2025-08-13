<?php

declare(strict_types=1);

function meetup_day(int $year, int $month, string $which, string $weekday): DateTimeImmutable
{
    $day_num = array(
        "Monday"    =>1,
        "Tuesday"   =>2,
        "Wednesday" =>3,
        "Thursday"  =>4,
        "Friday"    =>5,
        "Saturday"  =>6,
        "Sunday"    =>7
    );

    $first_day = array(
        "first"  =>1,
        "second" =>8,
        "third"  =>15,
        "fourth" =>22,
        "teenth" =>13
    );

    if ($which != "last") {
        $day = $first_day[$which];  // start day of requested week
        $dir = 1;
    } else {
        $day = date("t", strtotime("$year-$month-1")); // last day
        $dir = -1;
    }

    $guess = new DateTimeImmutable("$year-$month-$day");
    $offset = ($day_num[$weekday] - $guess->format("w") + 7 * $dir) % 7;
    return $guess->modify("$offset days");
}
