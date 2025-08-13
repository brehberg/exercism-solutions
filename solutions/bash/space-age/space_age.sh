#!/usr/bin/env bash
#
# Given an age in seconds, calculate how old someone would be
# on a planet in our Solar System.
set -eo pipefail

validate_args() {
    if ! [[ $# == 2 ]]; then
        echo "Usage: ${0##*/} <planet> <seconds>" >&2
        return 1
    fi
    if [[ -z ${ORBITAL_PERIOD[$1]} ]]; then
        echo "Error: $1 is not a planet" >&2
        return 1
    fi
    if [[ $2 =~ [^[:digit:]] ]]; then
        echo "Error: $2 is not a positive integer" >&2
        return 1
    fi
}

# One Earth year equals 365.25 Earth days, or 31,557,600 seconds.
declare -ri EARTH_YEAR=31557600
declare -rA ORBITAL_PERIOD=(
    [Mercury]=0.2408467
    [Venus]=0.61519726
    [Earth]=1
    [Mars]=1.8808158
    [Jupiter]=11.862615
    [Saturn]=29.447498
    [Uranus]=84.016846
    [Neptune]=164.79132
)

main() {
    validate_args "$@" || exit 1

    local planet="$1"
    local -i seconds="$2"

    compute_age() {
        bc -l <<<"$seconds / $EARTH_YEAR / ${ORBITAL_PERIOD[$planet]}"
    }

    printf "%.2f\n" "$(compute_age)"
}

main "$@"
