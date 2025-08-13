# Declare package 'Leap'
package Leap;

use v5.38;

use Exporter qw<import>;
our @EXPORT_OK = qw<is_leap_year>;

sub is_leap_year ($year) {
    my $is_divisible_by = sub ($n) 
        { $year % $n == 0 };

    return $is_divisible_by->(4)
        && !$is_divisible_by->(100)
        || $is_divisible_by->(400);
}
