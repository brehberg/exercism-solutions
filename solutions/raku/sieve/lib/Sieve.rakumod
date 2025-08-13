unit module Sieve;

sub find-primes ( $limit ) is export {

    sort keys [⊖] map { $_, 2×$_ ... $limit }, 2 .. $limit

}
