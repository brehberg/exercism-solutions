#!/usr/bin/env bash
#
# Implements the Sieve of Eratosthenes algorithm to find
# all prime numbers less than or equal to a given number.
set -eo pipefail

validate_args() {
    if ! [[ $# == 1 ]]; then
        echo "Usage: ${0##*/} <limit>" >&2
        return 1
    fi
    if [[ $1 =~ [^[:digit:]] ]]; then
        echo "Usage: ${0##*/} <limit>" >&2
        return 1
    fi
}

main() {
    validate_args "$@" || exit 1

    local limit="$1"

    local -a primes
    local -a marked
    for ((i = 0; i <= limit; i++)); do
        marked[i]=false
    done

    for ((n = 2; n <= limit; n++)); do
        if [[ "${marked[n]}" = true ]]; then
            continue
        fi
        primes+=("${n}")
        for ((m = n * n; m <= limit; m += n)); do
            marked[m]=true
        done
    done

    echo "${primes[@]}"
}

main "$@"
