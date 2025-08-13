unit module Leap;

sub is-leap-year ($year) is export {
    my $is-divisible-by = sub ($n)
        { $year %  $n == 0 };

    return $is-divisible-by(4) 
        && !$is-divisible-by(100) 
        || $is-divisible-by(400);
}
