#!/usr/bin/env tclsh

proc isLeapYear {year} {
    proc isDivisibleBy {n year} { expr $year % $n == 0 }
    expr [isDivisibleBy 4 $year] && ![isDivisibleBy 100 $year] || [isDivisibleBy 400 $year]
}
